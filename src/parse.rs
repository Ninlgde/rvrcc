//! AST parser
//! program = "{" compound_stmt
//! compoundStmt = (declaration | stmt)* "}"
//! declaration =
//!    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
//! declspec = "int"
//! declarator = "*"* ident
//! stmt = "return" expr ";"
//!        | "if" "(" expr ")" stmt ("else" stmt)?
//!        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//!        | "while" "(" expr ")" stmt
//!        | "{" compound_stmt
//!        | expr_stmt
//! expr_stmt = expr? ";"
//! expr = assign
//! assign = equality ("=" assign)?
//! equality = relational ("==" relational | "!=" relational)*
//! relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//! add = mul ("+" mul | "-" mul)*
//! mul = unary ("*" unary | "/" unary)*
//! unary = ("+" | "-" | "*" | "&") unary | primary
//! primary = "(" expr ")" | ident func-args? | num
//! func_call = ident "(" (assign ("," assign)*)? ")"

use std::slice::Iter;
use std::iter::{Enumerate, Peekable};
use crate::{error_token, Function, Node, Var, Token, Type, error_at};
use crate::ctype::add_type;
use crate::keywords::{KW_ELSE, KW_FOR, KW_IF, KW_RETURN, KW_WHILE};

pub fn parse(tokens: &Vec<Token>) -> Function {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    peekable: Peekable<Enumerate<Iter<'a, Token>>>,
    locals: Vec<Var>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            peekable: tokens.iter().enumerate().peekable(),
            locals: Vec::new(),
        }
    }

    /// 语法解析入口函数
    /// program = "{" compound_stmt
    pub fn parse(&mut self) -> Function {
        // "{"
        self.skip("{");

        // compound_stmt
        let node = self.compound_stmt().unwrap();

        // 计算栈总深度
        let offset: isize = (self.locals.len() * 8) as isize;
        // 构建返回值
        let program = Function {
            body: node,
            locals: self.locals.to_vec(),
            stack_size: align_to(offset, 16),
        };

        program
    }

    /// 解析复合语句
    /// compound_stmt =  (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Option<Node> {
        let mut nodes = vec![];
        let (pos, _) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();

        // 逐句解析,并压入nodes
        // (declaration | stmt)* "}"
        while let Some((_, token)) = self.peekable.peek() {
            if token.equal("}") {
                break;
            }
            let mut node;
            if token.equal("int") {
                // declaration
                node = self.declaration().unwrap();
            } else {
                // stmt
                node = self.stmt().unwrap();
            }
            add_type(&mut node);
            nodes.push(node);
        };

        let node = Node::Block { token: nt, body: nodes, type_: None };
        self.peekable.next();

        Some(node)
    }

    /// declaration =
    ///    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self) -> Option<Node> {
        // declspec
        // 声明的 基础类型
        let mut base_type = self.declspec();

        let mut nodes = vec![];
        let mut i = 0;

        // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)?
        while let Some((_, token)) = self.peekable.peek() {
            if token.equal(";") {
                break;
            }

            if i > 0 {
                self.skip(",");
            }
            i += 1;

            // declarator
            // 声明获取到变量类型，包括变量名
            let tuple = self.declarator(base_type);
            base_type = tuple.0;

            // 判断变量是否加入过locals,如果加入了,则不重复加入
            let obj = self.find_var(&tuple.1.clone());
            let nvar;
            if let Some(var) = obj {
                nvar = Var { name: tuple.1, offset: var.offset, type_: var.type_.clone() };
            } else {
                let offset: isize = -(((self.locals.len() + 1) * 8) as isize);
                nvar = Var { name: tuple.1, offset, type_: base_type.clone() };
                self.locals.push(nvar.clone());
            }

            let (pos, token) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
            if !token.equal("=") {
                continue;
            }

            // 解析“=”后面的Token
            let lhs = Some(Box::new(Node::Var { token: nt, var: Some(Box::new(nvar.clone())), type_: Some(base_type.clone()) }));
            // 解析递归赋值语句
            self.peekable.next();
            let rhs = Some(Box::new(self.assign().unwrap()));
            let (pos, _) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            let node = Some(Box::new(Node::Assign { token: nt.clone(), lhs, rhs, type_: None }));

            let node = Node::ExprStmt { token: nt.clone(), unary: node, type_: None };
            nodes.push(node);
        };

        let (pos, _) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        let node = Node::Block { token: nt, body: nodes, type_: None };
        self.peekable.next();

        Some(node)
    }

    /// declspec = "int"
    /// declarator specifier
    fn declspec(&mut self) -> Box<Type> {
        self.skip("int");
        Box::new(Type::Int {})
    }

    /// declarator = "*"* ident
    fn declarator(&mut self, mut type_: Box<Type>) -> (Box<Type>, String) {
        // "*"*
        // 构建所有的（多重）指针
        while self.consume("*") {
            type_ = Type::pointer_to(type_);
        }

        let (_, token) = self.peekable.peek().unwrap();
        let mut name = "".to_string();
        match token {
            Token::Ident { t_str, .. } => {
                name = t_str.to_string();
            }
            _ => {
                error_token!(token, "expected a variable name");
            }
        }

        // ident
        // 变量名
        self.peekable.next();
        (type_, name)
    }

    /// 解析语句
    /// stmt = "return" expr ";"
    ///        | "if" "(" expr ")" stmt ("else" stmt)?
    ///        | "for" "(" exprStmt expr? ";" expr? ")" stmt
    ///        | "while" "(" expr ")" stmt
    ///        | "{" compound_stmt
    ///        | expr_stmt
    fn stmt(&mut self) -> Option<Node> {
        let (pos, token) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        // "return" expr ";"
        if token.equal(KW_RETURN) {
            self.peekable.next();
            let node = Node::Return { token: nt, unary: Some(Box::new(self.expr().unwrap())), type_: None };
            self.skip(";");
            return Some(node);
        }

        // 解析if语句
        // "if" "(" expr ")" stmt ("else" stmt)?
        if token.equal(KW_IF) {
            // "(" expr ")"，条件内语句
            self.peekable.next();
            self.skip("(");
            let cond = Some(Box::new(self.expr().unwrap()));
            self.skip(")");
            // stmt，符合条件后的语句
            let then = Some(Box::new(self.stmt().unwrap()));
            // ("else" stmt)?，不符合条件后的语句
            let mut els = None;
            let (_, token) = self.peekable.peek().unwrap();
            if token.equal(KW_ELSE) {
                self.peekable.next();
                els = Some(Box::new(self.stmt().unwrap()));
            }
            return Some(Node::If { token: nt, cond, then, els, type_: None });
        }

        // | "for" "(" expr_stmt expr? ";" expr? ")" stmt
        if token.equal(KW_FOR) {
            // "("
            self.peekable.next();
            self.skip("(");
            //expr_stmt
            let init = Some(Box::new(self.expr_stmt().unwrap()));
            // expr?
            let mut cond = None;
            let (_, token) = self.peekable.peek().unwrap();
            if !token.equal(";") {
                cond = Some(Box::new(self.expr().unwrap()));
            }
            // ";"
            self.skip(";");
            // expr?
            let mut inc = None;
            let (_, token) = self.peekable.peek().unwrap();
            if !token.equal(")") {
                inc = Some(Box::new(self.expr().unwrap()));
            }
            // ")"
            self.skip(")");
            // stmt
            let then = Some(Box::new(self.stmt().unwrap()));

            return Some(Node::For { token: nt, init, inc, cond, then, type_: None });
        }

        // | "while" "(" expr ")" stmt
        if token.equal(KW_WHILE) {
            // "("
            self.peekable.next();
            self.skip("(");
            // expr
            let cond = Some(Box::new(self.expr().unwrap()));
            // ")"
            self.skip(")");
            // stmt
            let then = Some(Box::new(self.stmt().unwrap()));

            return Some(Node::For { token: nt, init: None, inc: None, cond, then, type_: None });
        }

        // "{" compound_stmt
        if token.equal("{") {
            self.peekable.next();
            return self.compound_stmt();
        }

        // expr_stmt
        self.expr_stmt()
    }

    /// 解析表达式语句
    /// expr_stmt = expr? ";"
    fn expr_stmt(&mut self) -> Option<Node> {
        // ;
        let (pos, token) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        if token.equal(";") {
            self.peekable.next();
            return Some(Node::Block { token: nt, body: vec![], type_: None });
        }

        // expr ";"
        let node = Node::ExprStmt { token: nt, unary: Some(Box::new(self.expr().unwrap())), type_: None };
        self.skip(";");

        Some(node)
    }

    /// 解析表达式
    /// expr = assign
    fn expr(&mut self) -> Option<Node> {
        // assign
        self.assign()
    }

    /// 解析赋值
    /// assign = equality ("=" assign)?
    fn assign(&mut self) -> Option<Node> {
        // equality
        let mut node = self.equality();

        // 可能存在递归赋值，如a=b=1
        // ("=" assign)?
        let (pos, token) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        if token.equal("=") {
            self.peekable.next();
            let rhs = Some(Box::new(self.assign().unwrap()));
            node = Some(Node::Assign { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None })
        }

        node
    }

    /// 解析相等性
    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Option<Node> {
        // relational
        let mut node = self.relational();

        // ("==" relational | "!=" relational)*
        loop {
            let (pos, token) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            // "==" relational
            if token.equal("==") {
                self.peekable.next();
                let rhs = Some(Box::new(self.relational().unwrap()));
                node = Some(Node::Eq { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                self.peekable.next();
                let rhs = Some(Box::new(self.relational().unwrap()));
                node = Some(Node::Ne { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            return node;
        }
    }

    /// 解析比较关系
    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Option<Node> {
        // add
        let mut node = self.add();

        // ("<" add | "<=" add | ">" add | ">=" add)*
        loop {
            let (pos, token) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            // "<" add
            if token.equal("<") {
                self.peekable.next();
                let rhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Lt { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "<=" add
            if token.equal("<=") {
                self.peekable.next();
                let rhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Le { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // ">" add
            // X>Y等价于Y<X
            if token.equal(">") {
                self.peekable.next();
                let lhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Lt { token: nt, lhs, rhs: Some(Box::new(node.unwrap())), type_: None });
                continue;
            }

            // ">=" add
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                self.peekable.next();
                let lhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Le { token: nt, lhs, rhs: Some(Box::new(node.unwrap())), type_: None });
                continue;
            }

            return node;
        }
    }

    // 解析各种type的加法
    fn add_with_type(&mut self, mut lhs: Option<Box<Node>>, mut rhs: Option<Box<Node>>, nt: Token) -> Option<Node> {
        // 为左右部添加类型
        add_type(lhs.as_mut().unwrap());
        add_type(rhs.as_mut().unwrap());

        let lhs_t = lhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
        let rhs_t = rhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
        // num + num
        if lhs_t.is_int() && rhs_t.is_int() {
            return Some(Node::Add { token: nt, lhs, rhs, type_: None });
        }

        // 不能解析 ptr + ptr
        if lhs_t.is_ptr() && rhs_t.is_ptr() {
            error_token!(&nt, "invalid operands");
            return None;
        }

        // 将 num + ptr 转换为 ptr + num
        let n_lhs;
        let n_rhs;
        if lhs_t.is_ptr() && rhs_t.is_ptr() {
            n_lhs = rhs;
            n_rhs = lhs;
        } else {
            n_lhs = lhs;
            n_rhs = rhs;
        }

        // ptr + num
        // 指针加法，ptr+1，这里的1不是1个字节，而是1个元素的空间，所以需要 ×8 操作
        // riscv 的变量是从大往小排列的,所以这里是-8
        let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: -8, type_: None }));
        let f_rhs = Some(Box::new(Node::Mul { token: nt.clone(), lhs: n_rhs, rhs: num8, type_: None }));
        Some(Node::Add { token: nt, lhs: n_lhs, rhs: f_rhs, type_: None })
    }

    // 解析各种type的减法
    fn sub_with_type(&mut self, mut lhs: Option<Box<Node>>, mut rhs: Option<Box<Node>>, nt: Token) -> Option<Node> {
        // 为左右部添加类型
        add_type(lhs.as_mut().unwrap());
        add_type(rhs.as_mut().unwrap());

        let lhs_t = lhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
        let rhs_t = rhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
        // num + num
        if lhs_t.is_int() && rhs_t.is_int() {
            return Some(Node::Sub { token: nt, lhs, rhs, type_: None });
        }

        // ptr - num
        if lhs_t.is_ptr() && rhs_t.is_int() {
            // riscv 的变量是从大往小排列的,所以这里是-8
            let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: -8, type_: None }));
            let mut f_rhs = Some(Box::new(Node::Mul { token: nt.clone(), lhs: rhs, rhs: num8, type_: None }));
            add_type(f_rhs.as_mut().unwrap());
            return Some(Node::Sub { token: nt, lhs, rhs: f_rhs, type_: Some(lhs_t) });
        }

        // ptr - ptr，返回两指针间有多少元素
        if lhs_t.is_ptr() && rhs_t.is_ptr() {
            let node = Some(Box::new(Node::Sub { token: nt.clone(), lhs, rhs, type_: Some(Box::new(Type::Int {})) }));
            // riscv 的变量是从大往小排列的,所以这里是-8
            let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: -8, type_: Some(Box::new(Type::Int {})) }));
            return Some(Node::Div { token: nt, lhs: node, rhs: num8, type_: None });
        }

        error_token!(&nt, "invalid operands");
        return None;
    }

    /// 解析加减
    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Option<Node> {
        // mul
        let mut node = self.mul();

        // ("+" mul | "-" mul)*
        loop {
            let (pos, token) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            // "+" mul
            if token.equal("+") {
                self.peekable.next();
                let rhs = Some(Box::new(self.mul().unwrap()));
                node = self.add_with_type(Some(Box::new(node.unwrap())), rhs, nt);
                continue;
            }

            // "-" mul
            if token.equal("-") {
                self.peekable.next();
                let rhs = Some(Box::new(self.mul().unwrap()));
                node = self.sub_with_type(Some(Box::new(node.unwrap())), rhs, nt);
                continue;
            }

            return node;
        }
    }

    /// 解析乘除
    /// mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Option<Node> {
        // unary
        let mut node = self.unary();

        // ("*" unary | "/" unary)*
        loop {
            let (pos, token) = self.peekable.peek().unwrap();
            let nt = self.tokens[*pos].clone();
            // "*" unary
            if token.equal("*") {
                self.peekable.next();
                let rhs = Some(Box::new(self.unary().unwrap()));
                node = Some(Node::Mul { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "/" unary
            if token.equal("/") {
                self.peekable.next();
                let rhs = Some(Box::new(self.unary().unwrap()));
                node = Some(Node::Div { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            return node;
        }
    }

    /// 解析一元运算
    /// unary = ("+" | "-" | "*" | "&") unary | primary
    fn unary(&mut self) -> Option<Node> {
        let (pos, token) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();

        // "+" unary
        if token.equal("+") {
            self.peekable.next();
            return self.unary();
        }

        // "-" unary
        if token.equal("-") {
            self.peekable.next();
            return Some(Node::Neg { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // "&" unary
        if token.equal("&") {
            self.peekable.next();
            return Some(Node::Addr { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // "*" unary
        if token.equal("*") {
            self.peekable.next();
            return Some(Node::DeRef { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // primary
        self.primary()
    }

    /// 解析括号、数字、变量
    /// primary = "(" expr ")" | ident args? | num
    fn primary(&mut self) -> Option<Node> {
        // "(" expr ")"
        let (pos, token) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        if token.equal("(") {
            self.peekable.next();
            let node = self.expr();
            self.skip(")");
            return node;
        }

        // ident args?
        match token {
            Token::Ident { t_str, offset } => {
                // 函数调用
                // args = "(" ")"
                if self.tokens[*pos + 1].equal("(") {
                    return self.func_call(t_str.to_string());
                }

                // ident
                // 查找变量
                let obj = self.find_var(t_str);
                let node;
                if let Some(var) = obj {
                    let nvar = Var { name: t_str.to_string(), offset: var.offset, type_: var.type_.clone() };
                    node = Node::Var { token: nt, var: Some(Box::new(nvar)), type_: None };
                } else {
                    error_at!(*offset, "undefined variable");
                    return None;
                }
                self.peekable.next();
                return Some(node);
            }
            Token::Num { val, t_str: _t_str, offset: _offset } => {
                let node = Node::Num { token: nt, val: *val, type_: None };
                self.peekable.next();
                return Some(node);
            }
            _ => {}
        }

        let (_, token) = self.peekable.peek().unwrap();
        error_token!(token, "expected an expression");

        None
    }

    // func_call = ident "(" (assign ("," assign)*)? ")"
    fn func_call(&mut self, func_name: String) -> Option<Node> {
        let (pos, _) = self.peekable.peek().unwrap();
        let nt = self.tokens[*pos].clone();
        self.peekable.next(); // 跳到(
        self.peekable.next(); // 调到参数或者)

        let mut nodes = vec![];

        loop {
            let (_, token) = self.peekable.peek().unwrap();
            if token.equal(")") {
                break;
            }
            if nodes.len() != 0 {
                self.skip(",");
            }
            let node = self.assign().unwrap();
            nodes.push(node);
        }

        self.skip(")");

        let node = Node::FuncCall { token: nt, func_name, args: nodes, type_: None };
        return Some(node);
    }

    fn find_var(&self, name: &String) -> Option<&Var> {
        self.locals.iter().find(|item| { item.name == *name })
    }

    fn skip(&mut self, s: &str) {
        let (_, token) = self.peekable.peek().unwrap();
        if !token.equal(s) {
            error_token!(token, "expect '{}'", s);
        }
        self.peekable.next();
    }

    fn consume(&mut self, s: &str) -> bool {
        let (_, token) = self.peekable.peek().unwrap();
        if token.equal(s) {
            self.peekable.next();
            return true;
        }
        return false;
    }
}

// 对齐到Align的整数倍
fn align_to(n: isize, align: isize) -> isize {
    // (0,Align]返回Align
    (n + align - 1) / align * align
}