//! AST parser
//! program = (function_definition* | global-variable)*
//! function_definition = declspec declarator "(" ")" "{" compound_stmt*
//! declspec = "char" | "int" | struct_declare | union_declare
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
//! expr = assign ("," expr)?
//! assign = equality ("=" assign)?
//! equality = relational ("==" relational | "!=" relational)*
//! relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//! add = mul ("+" mul | "-" mul)*
//! mul = unary ("*" unary | "/" unary)*
//! unary = ("+" | "-" | "*" | "&") unary | postfix
//! struct_members = (declspec declarator (","  declarator)* ";")*
//! struct_declare = struct_union_declare
//! union_declare = struct_union_declare
//! struct_union_declare = ident? ("{" struct_members)?
//! postfix = primary ("[" expr "]" | "." ident)* | "->" ident)*
//! primary =  "(" "{" stmt+ "}" ")"
//!         | "(" expr ")"
//!         | "sizeof" unary
//!         | ident funcArgs?
//!         | str
//!         | num
//! func_call = ident "(" (assign ("," assign)*)? ")"

use crate::ctype::{add_type, TypeKind};
use crate::keywords::{
    KW_CHAR, KW_ELSE, KW_FOR, KW_IF, KW_INT, KW_RETURN, KW_SIZEOF, KW_STRUCT, KW_UNION, KW_WHILE,
};
use crate::node::NodeKind;
use crate::obj::{Member, Scope};
use crate::{align_to, error_at, error_token, Node, Obj, Token, Type};
use std::cell::RefCell;
use std::rc::Rc;

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
            let gvar = Rc::new(RefCell::new(Obj::new_gvar(
                name.to_string(),
                base_type.clone(),
                None,
            )));

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
            base_type,
        )));
        // 结束当前域
        self.leave_scope();
        self.globals.push(function);
    }

    fn push_scope(&mut self, name: String, var: Rc<RefCell<Obj>>) {
        self.scopes[0].add_var(name, var);
    }

    fn push_tag_scope(&mut self, name: String, type_: Box<Type>) {
        self.scopes[0].add_tag(name, type_);
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

    /// declspec = "char" | "int" | struct_declare | union_declare
    /// declarator specifier
    fn declspec(&mut self) -> Box<Type> {
        let (_, token) = self.current();
        // "char"
        if token.equal(KW_CHAR) {
            self.next();
            return Type::new_char();
        }
        // "int"
        if token.equal(KW_INT) {
            self.next();
            return Type::new_int();
        }
        // struct_declare
        if token.equal(KW_STRUCT) {
            self.next();
            return self.struct_declare();
        }

        if token.equal(KW_UNION) {
            self.next();
            return self.union_declare();
        }

        error_token!(token, "typename expected");
        unreachable!()
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
        }

        let node = Node::new_block(NodeKind::Block, nodes, nt);
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
            let mut node = Node::new_var(nvar.clone(), nt);
            node.set_type(base_type.clone());
            let lhs = Box::new(node);
            // 解析递归赋值语句
            self.next();
            let rhs = Box::new(self.assign().unwrap());
            let (pos, _) = self.current();
            let nt = self.tokens[pos].clone();
            let expr = Box::new(Node::new_binary(NodeKind::Assign, lhs, rhs, nt.clone()));
            let node = Node::new_unary(NodeKind::ExprStmt, expr, nt);
            nodes.push(node);
        }

        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let node = Node::new_block(NodeKind::Block, nodes, nt);
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
            let node = Node::new_unary(NodeKind::Return, Box::new(self.expr().unwrap()), nt);
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
            let mut node = Node::new(NodeKind::If, nt);
            node.cond = cond;
            node.then = then;
            node.els = els;
            return Some(node);
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

            let mut node = Node::new(NodeKind::For, nt);
            node.init = init;
            node.inc = inc;
            node.cond = cond;
            node.then = then;
            return Some(node);
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

            let mut node = Node::new(NodeKind::For, nt);
            node.cond = cond;
            node.then = then;
            return Some(node);
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
            return Some(Node::new_block(NodeKind::Block, vec![], nt));
        }

        // expr ";"
        let node = Node::new_unary(NodeKind::ExprStmt, Box::new(self.expr().unwrap()), nt);
        self.skip(";");

        Some(node)
    }

    /// 解析表达式
    /// expr = assign ("," expr)?
    fn expr(&mut self) -> Option<Node> {
        // assign
        let node = self.assign().unwrap();

        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal(",") {
            self.next();
            let rhs = Box::new(self.expr().unwrap());
            return Some(Node::new_binary(NodeKind::Comma, Box::new(node), rhs, nt));
        }

        return Some(node);
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
            let rhs = Box::new(self.assign().unwrap());
            node = Some(Node::new_binary(
                NodeKind::Assign,
                Box::new(node.unwrap()),
                rhs,
                nt,
            ));
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
                let rhs = Box::new(self.relational().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Eq,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                self.next();
                let rhs = Box::new(self.relational().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Ne,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
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
                let rhs = Box::new(self.add().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Lt,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // "<=" add
            if token.equal("<=") {
                self.next();
                let rhs = Box::new(self.add().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Le,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // ">" add
            // X>Y等价于Y<X
            if token.equal(">") {
                self.next();
                let lhs = Box::new(self.add().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Lt,
                    lhs,
                    Box::new(node.unwrap()),
                    nt,
                ));
                continue;
            }

            // ">=" add
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                self.next();
                let lhs = Box::new(self.add().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Le,
                    lhs,
                    Box::new(node.unwrap()),
                    nt,
                ));
                continue;
            }

            return node;
        }
    }

    // 解析各种type的加法
    fn add_with_type(&mut self, mut lhs: Box<Node>, mut rhs: Box<Node>, nt: Token) -> Option<Node> {
        // 为左右部添加类型
        add_type(lhs.as_mut());
        add_type(rhs.as_mut());

        let lhs_t = lhs.get_type().as_ref().unwrap().clone();
        let rhs_t = rhs.get_type().as_ref().unwrap().clone();
        // num + num
        if lhs_t.is_int() && rhs_t.is_int() {
            return Some(Node::new_binary(NodeKind::Add, lhs, rhs, nt));
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
        let size = Box::new(Node::new_num(size, nt.clone()));
        let f_rhs = Box::new(Node::new_binary(NodeKind::Mul, n_rhs, size, nt.clone()));
        Some(Node::new_binary(NodeKind::Add, n_lhs, f_rhs, nt))
    }

    // 解析各种type的减法
    fn sub_with_type(&mut self, mut lhs: Box<Node>, mut rhs: Box<Node>, nt: Token) -> Option<Node> {
        // 为左右部添加类型
        add_type(lhs.as_mut());
        add_type(rhs.as_mut());

        let lhs_t = lhs.get_type().as_ref().unwrap().clone();
        let rhs_t = rhs.get_type().as_ref().unwrap().clone();
        // num + num
        if lhs_t.is_int() && rhs_t.is_int() {
            return Some(Node::new_binary(NodeKind::Sub, lhs, rhs, nt));
        }

        // ptr - num
        if lhs_t.has_base() && rhs_t.is_int() {
            let size: i32 = lhs_t.get_base_size() as i32;
            let size = Box::new(Node::new_num(size, nt.clone()));
            let mut f_rhs = Box::new(Node::new_binary(NodeKind::Mul, rhs, size, nt.clone()));
            add_type(f_rhs.as_mut());
            let mut node = Node::new_binary(NodeKind::Sub, lhs, f_rhs, nt);
            node.set_type(lhs_t);
            return Some(node);
        }

        // ptr - ptr，返回两指针间有多少元素
        if lhs_t.has_base() && rhs_t.has_base() {
            let mut node = Box::new(Node::new_binary(NodeKind::Sub, lhs, rhs, nt.clone()));
            node.set_type(Type::new_int());
            let size: i32 = lhs_t.get_base_size() as i32;
            let mut size = Box::new(Node::new_num(size, nt.clone()));
            size.set_type(Type::new_int());
            return Some(Node::new_binary(NodeKind::Div, node, size, nt));
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
                let rhs = Box::new(self.mul().unwrap());
                node = self.add_with_type(Box::new(node.unwrap()), rhs, nt);
                continue;
            }

            // "-" mul
            if token.equal("-") {
                self.next();
                let rhs = Box::new(self.mul().unwrap());
                node = self.sub_with_type(Box::new(node.unwrap()), rhs, nt);
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
                let rhs = Box::new(self.unary().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Mul,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // "/" unary
            if token.equal("/") {
                self.next();
                let rhs = Box::new(self.unary().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Div,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
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
            return Some(Node::new_unary(
                NodeKind::Neg,
                Box::new(self.unary().unwrap()),
                nt,
            ));
        }

        // "&" unary
        if token.equal("&") {
            self.next();
            return Some(Node::new_unary(
                NodeKind::Addr,
                Box::new(self.unary().unwrap()),
                nt,
            ));
        }

        // "*" unary
        if token.equal("*") {
            self.next();
            return Some(Node::new_unary(
                NodeKind::DeRef,
                Box::new(self.unary().unwrap()),
                nt,
            ));
        }

        // primary
        self.postfix()
    }

    /// struct_members = (declspec declarator (","  declarator)* ";")*
    fn struct_members(&mut self, type_: &mut Box<Type>) {
        let mut members = vec![];

        loop {
            let (_, token) = self.current();
            if token.equal("}") {
                break;
            }

            let mut base_type = self.declspec();
            let mut is_first = true;

            while !self.consume(";") {
                if !is_first {
                    self.skip(",");
                }
                is_first = false;

                base_type = self.declarator(base_type);
                let name = base_type.get_name().to_string();
                let member = Member::new(name.to_string(), Some(base_type.clone()));
                members.push(member);
            }
        }

        self.next();
        type_.members = members;
    }

    /// struct_declare = struct_union_declare
    fn struct_declare(&mut self) -> Box<Type> {
        let mut type_ = self.struct_union_declare();
        type_.kind = TypeKind::Struct;

        let mut offset: usize = 0;

        for member in &mut type_.members {
            let align = member.type_.as_ref().unwrap().align;
            offset = align_to(offset as isize, align as isize) as usize;
            member.set_offset(offset);
            offset += member.type_.as_ref().unwrap().size;

            if type_.align < align {
                type_.align = align;
            }
        }
        type_.size = align_to(offset as isize, type_.align as isize) as usize;

        type_
    }

    /// union_declare = struct_union_declare
    fn union_declare(&mut self) -> Box<Type> {
        let mut type_ = self.struct_union_declare();
        type_.kind = TypeKind::Union;

        // 联合体需要设置为最大的对齐量与大小，变量偏移量都默认为0
        for member in &mut type_.members {
            let align = member.type_.as_ref().unwrap().align;
            let size = member.type_.as_ref().unwrap().size;
            if type_.align < align {
                type_.align = align;
            }
            if type_.size < size {
                type_.size = size;
            }
        }
        type_.size = align_to(type_.size as isize, type_.align as isize) as usize;

        type_
    }

    /// struct_union_declare = ident? ("{" struct_members)?
    fn struct_union_declare(&mut self) -> Box<Type> {
        let mut tag = None;
        let (_, tag_token) = self.current();
        if tag_token.is_ident() {
            tag = Some(tag_token.clone());
            self.next();
        }

        let (_, token) = self.current();
        if tag.is_some() && !token.equal("{") {
            let type_ = self.find_tag(&tag.as_ref().unwrap().get_name().to_string());
            if type_.is_none() {
                error_token!(&tag.unwrap(), "unknown struct type");
                unreachable!()
            }
            return type_.unwrap();
        }

        let mut type_ = Type::new_union_struct();
        self.next();
        self.struct_members(&mut type_);
        type_.align = 1;

        // 如果有名称就注册结构体类型
        if tag.is_some() {
            self.push_tag_scope(tag.unwrap().get_name(), type_.clone());
        }
        type_
    }

    // 获取结构体成员
    fn get_struct_member(&mut self, type_: &Box<Type>, token: &Token) -> Option<Member> {
        for member in type_.members.iter() {
            if token.equal(member.name.as_str()) {
                return Some(member.clone());
            }
        }
        error_token!(token, "no such member");
        None
    }

    /// 构建结构体成员的节点
    fn struct_ref(&mut self, mut lhs: Box<Node>) -> Option<Node> {
        add_type(lhs.as_mut());

        let lhs_t = lhs.get_type().as_ref().unwrap().clone();
        if lhs_t.kind != TypeKind::Struct && lhs_t.kind != TypeKind::Union {
            error_token!(&lhs.as_ref().token, "not a struct nor a union");
        }

        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let mut node = Node::new_unary(NodeKind::Member, lhs, nt.clone());
        node.member = Some(Box::new(self.get_struct_member(&lhs_t, &nt).unwrap()));

        Some(node)
    }

    /// postfix = primary ("[" expr "]" | "." ident)* | "->" ident)*
    fn postfix(&mut self) -> Option<Node> {
        // primary
        let mut node = self.primary().unwrap();

        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // ("[" expr "]")*
            if token.equal("[") {
                // x[y] 等价于 *(x+y)
                self.next();
                let idx = self.expr().unwrap();
                self.skip("]");
                let unary = self
                    .add_with_type(Box::new(node), Box::new(idx), nt.clone())
                    .unwrap();
                node = Node::new_unary(NodeKind::DeRef, Box::new(unary), nt);
                continue;
            }

            // "." ident
            if token.equal(".") {
                self.next();
                node = self.struct_ref(Box::new(node)).unwrap();
                self.next();
                continue;
            }

            // "->" ident
            if token.equal("->") {
                node = Node::new_unary(NodeKind::DeRef, Box::new(node), nt);
                self.next();
                node = self.struct_ref(Box::new(node)).unwrap();
                self.next();
                continue;
            }

            break;
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
            let body = self.compound_stmt().unwrap().body.to_vec();
            let node = Node::new_block(NodeKind::StmtExpr, body, nt);
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
            let size = node.unwrap().get_type().as_ref().unwrap().get_size() as i32;
            return Some(Node::new_num(size, nt));
        }

        // ident args?
        match token {
            Token::Ident {
                t_str,
                offset,
                line_no,
            } => {
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
                    node = Node::new_var(var.clone(), nt);
                } else {
                    error_at!(*line_no, *offset, "undefined variable");
                    return None;
                }
                self.next();
                return Some(node);
            }
            Token::Str { val, type_, .. } => {
                let var = self.new_string_literal(val.to_vec(), type_.clone());
                let node = Node::new_var(var, nt);
                self.next();
                return Some(node);
            }
            Token::Num {
                val,
                t_str: _t_str,
                offset: _offset,
                ..
            } => {
                let node = Node::new_num(*val, nt);
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

        let mut node = Node::new(NodeKind::FuncCall, nt);
        node.func_name = func_name;
        node.args = nodes;
        Some(node)
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

    /// 通过名称，查找tag
    fn find_tag(&self, name: &String) -> Option<Box<Type>> {
        for scope in self.scopes.iter() {
            if let Some(tag) = scope.get_tag(name) {
                return Some(tag.clone());
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
            Token::Num { val, .. } => *val,
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
        return token.equal(KW_INT)
            || token.equal(KW_CHAR)
            || token.equal(KW_STRUCT)
            || token.equal(KW_UNION);
    }

    fn new_string_literal(&mut self, str_data: Vec<u8>, base_type: Box<Type>) -> Rc<RefCell<Obj>> {
        let name = format!(".L..{}", self.unique_idx);
        self.unique_idx += 1;
        let gvar = Rc::new(RefCell::new(Obj::new_gvar(
            name.to_string(),
            base_type,
            Some(str_data),
        )));
        // 加入globals
        self.globals.push(gvar.clone());
        self.push_scope(name.to_string(), gvar.clone());
        gvar
    }
}
