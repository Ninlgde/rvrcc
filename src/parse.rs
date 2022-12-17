//! AST parser
//! program = (function_definition* | global-variable)*
//! function_definition = declspec declarator "(" ")" "{" compound_stmt*
//! declspec = "char" | "int"
//! declarator = "*"* ident type_suffix
//! type_suffix = "(" func_params | "[" num "]" type_suffix | ε
//! func_params = (param ("," param)*)? ")"
//! param = declspec declarator
//! compound_stmt = (declaration | stmt)* "}"
//! declaration =
//!    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
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
//! unary = ("+" | "-" | "*" | "&") unary | postfix
//! postfix = primary ("[" expr "]")*
//! primary =  "(" "{" stmt+ "}" ")"
//!         | "(" expr ")"
//!         | "sizeof" unary
//!         | ident funcArgs?
//!         | str
//!         | num
//! func_call = ident "(" (assign ("," assign)*)? ")"

use std::cell::RefCell;
use std::rc::Rc;
use crate::{error_token, Node, Obj, Token, Type, error_at};
use crate::ctype::add_type;
use crate::keywords::{KW_CHAR, KW_ELSE, KW_FOR, KW_IF, KW_INT, KW_RETURN, KW_SIZEOF, KW_WHILE};
use crate::obj::Scope;

pub fn parse(tokens: &Vec<Token>) -> Vec<Rc<RefCell<Obj>>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    cursor: usize,
    // 本地变量
    locals: Vec<Rc<RefCell<Obj>>>,
    // 全局变量
    globals: Vec<Rc<RefCell<Obj>>>,
    // 唯一名称idx
    unique_idx: usize,
    // 变量域
    scopes: Vec<Scope>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            locals: Vec::new(),
            globals: Vec::new(),
            unique_idx: 0,
            scopes: vec![Scope::new()],
        }
    }

    /// 获取当前游标和游标所指的token
    fn current(&self) -> (usize, &Token) {
        (self.cursor, &self.tokens[self.cursor])
    }

    // 指向下一个
    fn next(&mut self) -> &mut Self {
        self.cursor += 1;
        self
    }

    /// 语法解析入口函数
    /// program = function_definition*
    pub fn parse(&mut self) -> Vec<Rc<RefCell<Obj>>> {
        // "{"
        loop {
            let (_, token) = self.current();
            if token.at_eof() {
                break;
            }
            // declspec
            let base_type = self.declspec();

            if self.is_function() {
                self.function_definition(base_type);
                continue;
            }

            self.global_variable(base_type)
        }

        self.globals.to_vec()
    }

    fn global_variable(&mut self, mut base_type: Box<Type>) {
        let mut first = true;

        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;

            base_type = self.declarator(base_type);
            let name = base_type.get_name().to_string();
            let gvar = Rc::new(RefCell::new(Obj::new_gvar(name.to_string(), base_type.clone(), None)));

            self.globals.push(gvar.clone());
            self.push_scope(name.to_string(), gvar.clone());
        }
    }

    /// function_definition = declspec declarator "(" ")" "{" compound_stmt*
    fn function_definition(&mut self, mut base_type: Box<Type>) {
        // declarator
        // 声明获取到变量类型，包括变量名
        base_type = self.declarator(base_type);
        let name = base_type.get_name().to_string();

        // 本地变量清空
        self.locals.clear();
        // 进入新的域
        self.enter_scope();
        self.create_param_lvars(base_type.get_params());
        let params = self.locals.to_vec();

        self.skip("{");

        // compound_stmt
        let body = self.compound_stmt();

        let function = Rc::new(RefCell::new(Obj::new_func(
            name,
            params,
            self.locals.to_vec(),
            body,
            base_type)));
        // 结束当前域
        self.leave_scope();
        self.globals.push(function);
    }

    fn push_scope(&mut self, name: String, var: Rc<RefCell<Obj>>) {
        self.scopes[0].add_var(name, var);
    }

    /// 将形参添加到locals
    fn create_param_lvars(&mut self, params: Vec<Type>) {
        for param in params.iter() {
            self.new_lvar(Box::new(param.clone()));
        }
    }

    /// 创建新的左值
    fn new_lvar(&mut self, base_type: Box<Type>) -> Option<Rc<RefCell<Obj>>> {
        let name = base_type.get_name().to_string();
        let nvar = Rc::new(RefCell::new(Obj::new_lvar(name.to_string(), base_type)));
        self.locals.push(nvar.clone());
        self.push_scope(name.to_string(), nvar.clone());
        Some(nvar)
    }

    /// declspec = "char" | "int"
    /// declarator specifier
    fn declspec(&mut self) -> Box<Type> {
        let (_, token) = self.current();
        if token.equal(KW_CHAR) {
            self.next();
            return Type::new_char();
        }
        self.skip(KW_INT);
        Type::new_int()
    }

    /// declarator = "*"* ident type_suffix
    fn declarator(&mut self, mut type_: Box<Type>) -> Box<Type> {
        // "*"*
        // 构建所有的（多重）指针
        while self.consume("*") {
            type_ = Type::pointer_to(type_);
        }

        let (_, token) = self.current();
        let mut name = "".to_string();
        match token {
            Token::Ident { t_str, .. } => {
                name = t_str.to_string();
            }
            _ => {
                error_token!(token, "expected a variable name");
            }
        }

        // type_suffix
        self.next();
        type_ = self.type_suffix(type_);
        // ident
        // 变量名 或 函数名
        type_.set_name(name);
        type_
    }

    /// type_suffix = "(" func_params | "[" num "]" type_suffix | ε
    fn type_suffix(&mut self, mut type_: Box<Type>) -> Box<Type> {
        let (_, token) = self.current();
        // "(" func_params
        if token.equal("(") {
            self.next();
            return self.func_params(type_);
        }

        // "[" num "]"
        if token.equal("[") {
            self.next();
            let size = self.get_number();
            self.next(); // 跳过这个数字
            self.skip("]"); // 跳过]
            type_ = self.type_suffix(type_);
            return Type::array_of(type_, size as usize);
        }

        return type_;
    }

    /// func_params = (param ("," param)*)? ")"
    /// param = declspec declarator
    fn func_params(&mut self, type_: Box<Type>) -> Box<Type> {
        let mut params = vec![];
        loop {
            let (_, token) = self.current();
            if token.equal(")") {
                break;
            }
            // funcParams = param ("," param)*
            // param = declspec declarator
            if params.len() > 0 {
                self.skip(",");
            }

            let mut t = self.declspec();
            t = self.declarator(t);

            // 倒序插入
            params.insert(0, *t);
        }
        self.skip(")");

        return Type::func_type(type_, params);
    }

    /// 解析复合语句
    /// compound_stmt =  (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Option<Node> {
        let mut nodes = vec![];
        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();

        // 进入新的域
        self.enter_scope();

        // 逐句解析,并压入nodes
        // (declaration | stmt)* "}"
        loop {
            let (_, token) = self.current();
            if token.equal("}") {
                break;
            }
            let mut node;
            if self.is_type_name() {
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
        self.next();

        // 结束当前的域
        self.leave_scope();

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
        loop {
            let (_, token) = self.current();
            if token.equal(";") {
                break;
            }

            if i > 0 {
                self.skip(",");
            }
            i += 1;

            // declarator
            // 声明获取到变量类型，包括变量名
            base_type = self.declarator(base_type);
            let nvar = self.new_lvar(base_type.clone()).unwrap();

            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
            if !token.equal("=") {
                continue;
            }

            // 解析“=”后面的Token
            let lhs = Some(Box::new(Node::Var { token: nt, var: Some(nvar.clone()), type_: Some(base_type.clone()) }));
            // 解析递归赋值语句
            self.next();
            let rhs = Some(Box::new(self.assign().unwrap()));
            let (pos, _) = self.current();
            let nt = self.tokens[pos].clone();
            let node = Some(Box::new(Node::Assign { token: nt.clone(), lhs, rhs, type_: None }));

            let node = Node::ExprStmt { token: nt.clone(), unary: node, type_: None };
            nodes.push(node);
        };

        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let node = Node::Block { token: nt, body: nodes, type_: None };
        self.next();

        Some(node)
    }

    /// 解析语句
    /// stmt = "return" expr ";"
    ///        | "if" "(" expr ")" stmt ("else" stmt)?
    ///        | "for" "(" exprStmt expr? ";" expr? ")" stmt
    ///        | "while" "(" expr ")" stmt
    ///        | "{" compound_stmt
    ///        | expr_stmt
    fn stmt(&mut self) -> Option<Node> {
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        // "return" expr ";"
        if token.equal(KW_RETURN) {
            self.next();
            let node = Node::Return { token: nt, unary: Some(Box::new(self.expr().unwrap())), type_: None };
            self.skip(";");
            return Some(node);
        }

        // 解析if语句
        // "if" "(" expr ")" stmt ("else" stmt)?
        if token.equal(KW_IF) {
            // "(" expr ")"，条件内语句
            self.next();
            self.skip("(");
            let cond = Some(Box::new(self.expr().unwrap()));
            self.skip(")");
            // stmt，符合条件后的语句
            let then = Some(Box::new(self.stmt().unwrap()));
            // ("else" stmt)?，不符合条件后的语句
            let mut els = None;
            let (_, token) = self.current();
            if token.equal(KW_ELSE) {
                self.next();
                els = Some(Box::new(self.stmt().unwrap()));
            }
            return Some(Node::If { token: nt, cond, then, els, type_: None });
        }

        // | "for" "(" expr_stmt expr? ";" expr? ")" stmt
        if token.equal(KW_FOR) {
            // "("
            self.next();
            self.skip("(");
            //expr_stmt
            let init = Some(Box::new(self.expr_stmt().unwrap()));
            // expr?
            let mut cond = None;
            let (_, token) = self.current();
            if !token.equal(";") {
                cond = Some(Box::new(self.expr().unwrap()));
            }
            // ";"
            self.skip(";");
            // expr?
            let mut inc = None;
            let (_, token) = self.current();
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
            self.next();
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
            self.next();
            return self.compound_stmt();
        }

        // expr_stmt
        self.expr_stmt()
    }

    /// 解析表达式语句
    /// expr_stmt = expr? ";"
    fn expr_stmt(&mut self) -> Option<Node> {
        // ;
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal(";") {
            self.next();
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
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal("=") {
            self.next();
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
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "==" relational
            if token.equal("==") {
                self.next();
                let rhs = Some(Box::new(self.relational().unwrap()));
                node = Some(Node::Eq { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                self.next();
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
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "<" add
            if token.equal("<") {
                self.next();
                let rhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Lt { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "<=" add
            if token.equal("<=") {
                self.next();
                let rhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Le { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // ">" add
            // X>Y等价于Y<X
            if token.equal(">") {
                self.next();
                let lhs = Some(Box::new(self.add().unwrap()));
                node = Some(Node::Lt { token: nt, lhs, rhs: Some(Box::new(node.unwrap())), type_: None });
                continue;
            }

            // ">=" add
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                self.next();
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
        if lhs_t.has_base() && rhs_t.has_base() {
            error_token!(&nt, "invalid operands");
            return None;
        }

        // 将 num + ptr 转换为 ptr + num
        let n_lhs;
        let n_rhs;
        let size;
        if !lhs_t.has_base() && rhs_t.has_base() {
            n_lhs = rhs;
            n_rhs = lhs;
            size = lhs_t.get_size() as i32;
        } else {
            n_lhs = lhs;
            n_rhs = rhs;
            size = lhs_t.get_base_size() as i32;
        }

        // ptr + num
        // 指针加法，ptr+1，这里的1不是1个字节，而是1个元素的空间，所以需要 ×size 操作
        let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: size, type_: None }));
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
        if lhs_t.has_base() && rhs_t.is_int() {
            let size: i32 = lhs_t.get_base_size() as i32;
            let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: size, type_: None }));
            let mut f_rhs = Some(Box::new(Node::Mul { token: nt.clone(), lhs: rhs, rhs: num8, type_: None }));
            add_type(f_rhs.as_mut().unwrap());
            return Some(Node::Sub { token: nt, lhs, rhs: f_rhs, type_: Some(lhs_t) });
        }

        // ptr - ptr，返回两指针间有多少元素
        if lhs_t.has_base() && rhs_t.has_base() {
            let node = Some(Box::new(Node::Sub { token: nt.clone(), lhs, rhs, type_: Some(Type::new_int()) }));
            let size: i32 = lhs_t.get_base_size() as i32;
            let num8 = Some(Box::new(Node::Num { token: nt.clone(), val: size, type_: Some(Type::new_int()) }));
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
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "+" mul
            if token.equal("+") {
                self.next();
                let rhs = Some(Box::new(self.mul().unwrap()));
                node = self.add_with_type(Some(Box::new(node.unwrap())), rhs, nt);
                continue;
            }

            // "-" mul
            if token.equal("-") {
                self.next();
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
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "*" unary
            if token.equal("*") {
                self.next();
                let rhs = Some(Box::new(self.unary().unwrap()));
                node = Some(Node::Mul { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            // "/" unary
            if token.equal("/") {
                self.next();
                let rhs = Some(Box::new(self.unary().unwrap()));
                node = Some(Node::Div { token: nt, lhs: Some(Box::new(node.unwrap())), rhs, type_: None });
                continue;
            }

            return node;
        }
    }

    /// 解析一元运算
    /// unary = ("+" | "-" | "*" | "&") unary | postfix
    fn unary(&mut self) -> Option<Node> {
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();

        // "+" unary
        if token.equal("+") {
            self.next();
            return self.unary();
        }

        // "-" unary
        if token.equal("-") {
            self.next();
            return Some(Node::Neg { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // "&" unary
        if token.equal("&") {
            self.next();
            return Some(Node::Addr { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // "*" unary
        if token.equal("*") {
            self.next();
            return Some(Node::DeRef { token: nt, unary: Some(Box::new(self.unary().unwrap())), type_: None });
        }

        // primary
        self.postfix()
    }

    /// postfix = primary ("[" expr "]")*
    fn postfix(&mut self) -> Option<Node> {
        // primary
        let mut node = self.primary().unwrap();

        // ("[" expr "]")*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("[") {
                break;
            }

            // x[y] 等价于 *(x+y)
            self.next();
            let idx = self.expr().unwrap();
            self.skip("]");
            let unary = self.add_with_type(Some(Box::new(node)), Some(Box::new(idx)), nt.clone()).unwrap();
            node = Node::DeRef { token: nt.clone(), unary: Some(Box::new(unary)), type_: None }
        }

        Some(node)
    }

    /// 解析括号、数字、变量
    /// primary = "(" "{" stmt+ "}" ")"
    ///         | "(" expr ")"
    ///         | "sizeof" unary
    ///         | ident funcArgs?
    ///         | str
    ///         | num
    fn primary(&mut self) -> Option<Node> {
        // "(" "{" stmt+ "}" ")"
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        let next = &self.tokens[pos + 1];
        if token.equal("(") && next.equal("{") {
            // This is a GNU statement expresssion.
            self.next().next();
            let body = self.compound_stmt().unwrap().get_body().to_vec();
            let node = Node::StmtExpr { token: nt, body, type_: None };
            self.skip(")");
            return Some(node);
        }

        // "(" expr ")"
        if token.equal("(") {
            self.next();
            let node = self.expr();
            self.skip(")");
            return node;
        }

        if token.equal(KW_SIZEOF) {
            self.next();
            let mut node = self.unary();
            add_type(node.as_mut().unwrap());
            let (pos, _) = self.current();
            let nt = self.tokens[pos].clone();
            return Some(Node::Num { token: nt, type_: None, val: node.as_ref().unwrap().get_type().as_ref().unwrap().get_size() as i32 });
        }

        // ident args?
        match token {
            Token::Ident { t_str, offset } => {
                // 函数调用
                // args = "(" ")"
                if self.tokens[pos + 1].equal("(") {
                    return self.func_call(t_str.to_string());
                }

                // ident
                // 查找变量
                let obj = self.find_var(&t_str);
                let node;
                if let Some(var) = obj {
                    node = Node::Var { token: nt, var: Some(var.clone()), type_: None };
                } else {
                    error_at!(*offset, "undefined variable");
                    return None;
                }
                self.next();
                return Some(node);
            }
            Token::Str { val, type_, .. } => {
                let var = self.new_string_literal(val.to_vec(), type_.clone());
                let node = Node::Var { token: nt, var: Some(var), type_: None };
                self.next();
                return Some(node);
            }
            Token::Num { val, t_str: _t_str, offset: _offset } => {
                let node = Node::Num { token: nt, val: *val, type_: None };
                self.next();
                return Some(node);
            }
            _ => {}
        }

        let (_, token) = self.current();
        error_token!(token, "expected an expression");

        None
    }

    // func_call = ident "(" (assign ("," assign)*)? ")"
    fn func_call(&mut self, func_name: String) -> Option<Node> {
        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        self.next().next(); // 1. 跳到(  2.调到参数或者)

        let mut nodes = vec![];

        loop {
            let (_, token) = self.current();
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

    fn enter_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.insert(0, scope);
    }

    fn leave_scope(&mut self) {
        self.scopes.remove(0);
    }

    /// 通过名称，查找一个变量
    fn find_var(&self, name: &String) -> Option<Rc<RefCell<Obj>>> {
        for scope in self.scopes.iter() {
            if let Some(var) = scope.get_var(name) {
                return Some(var.clone());
            }
        }
        return None;
    }

    fn skip(&mut self, s: &str) {
        let (_, token) = self.current();
        if !token.equal(s) {
            error_token!(token, "expect '{}'", s);
        }
        self.next();
    }

    fn consume(&mut self, s: &str) -> bool {
        let (_, token) = self.current();
        if token.equal(s) {
            self.next();
            return true;
        }
        return false;
    }

    fn get_number(&mut self) -> i32 {
        let (_, token) = self.current();
        return match token {
            Token::Num { val, .. } => {
                *val
            }
            _ => {
                error_token!(token, "expect a number");
                0 // 不会走到这里
            }
        };
    }

    /// 判断是否是方法,要回档哦
    fn is_function(&mut self) -> bool {
        let (start, token) = self.current();
        if token.equal(";") {
            return false;
        }
        let type_ = self.declarator(Type::new_int());
        // 游标回档
        self.cursor = start;
        return type_.is_func();
    }

    fn is_type_name(&self) -> bool {
        let (_, token) = self.current();
        return token.equal(KW_INT) || token.equal(KW_CHAR);
    }

    fn new_string_literal(&mut self, str_data: Vec<u8>, base_type: Box<Type>) -> Rc<RefCell<Obj>> {
        let name = format!(".L..{}", self.unique_idx);
        self.unique_idx += 1;
        let gvar = Rc::new(RefCell::new(Obj::new_gvar(name.to_string(), base_type, Some(str_data))));
        // 加入globals
        self.globals.push(gvar.clone());
        self.push_scope(name.to_string(), gvar.clone());
        gvar
    }
}