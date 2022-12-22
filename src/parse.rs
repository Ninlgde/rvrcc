//! AST parser
//! program = (typedef | function_definition* | global-variable)*
//! function_definition = declspec declarator "(" ")" "{" compound_stmt*
//! declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//!            | "typedef" | "static"
//!            | struct_declare | union_declare | typedef_name
//!            | enum_specifier)+
//! enum_specifier = ident? "{" enum_list? "}"
//!                 | ident ("{" enum_list? "}")?
//! enum_list = ident ("=" num)? ("," ident ("=" num)?)*
//! declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type_suffix
//! type_suffix = "(" funcParams | "[" array_dimensions | ε
//! array_dimensions = num? "]" typeSuffix
//! func_params = (param ("," param)*)? ")"
//! param = declspec declarator
//! compound_stmt = (typedef | declaration | stmt)* "}"
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
//! assign = log_or (assign_op assign)?
//! log_or = log_and ("||" log_and)*
//! log_and = bit_or ("&&" bit_or)*
//! bit_or = bit_xor ("|" bit_xor)*
//! bit_xor = bit_and ("^" bit_and)*
//! bit_and = equality ("&" equality)*
//! assign_op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//! equality = relational ("==" relational | "!=" relational)*
//! relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//! add = mul ("+" mul | "-" mul)*
//! mul = cast ("*" cast | "/" cast | "%" cast)*
//! cast = "(" typename ")" cast | unary
//! unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//!         | ("++" | "--") unary
//!         | postfix
//! struct_members = (declspec declarator (","  declarator)* ";")*
//! struct_declare = struct_union_declare
//! union_declare = struct_union_declare
//! struct_union_declare = ident? ("{" struct_members)?
//! postfix = primary ("[" expr "]" | "." ident)* | "->" ident | "++" | "--")*
//! primary =  "(" "{" stmt+ "}" ")"
//!         | "(" expr ")"
//!         | "sizeof" "(" typename ")"
//!         | "sizeof" unary
//!         | ident funcArgs?
//!         | str
//!         | num
//! typename = declspec abstract_declarator
//! abstract_declarator = "*"* ("(" abstract_declarator ")")? type_suffix
//! func_call = ident "(" (assign ("," assign)*)? ")"

use crate::ctype::{add_type, TypeKind};
use crate::keywords::{
    KW_BOOL, KW_CHAR, KW_ELSE, KW_ENUM, KW_FOR, KW_IF, KW_INT, KW_LONG, KW_RETURN, KW_SHORT,
    KW_SIZEOF, KW_STATIC, KW_STRUCT, KW_TYPEDEF, KW_UNION, KW_VOID, KW_WHILE,
};
use crate::node::NodeKind;
use crate::obj::{Member, Scope, VarAttr, VarScope};
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
    // 当前正在解析的function
    cur_func: Option<Rc<RefCell<Obj>>>,
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
            cur_func: None,
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
            let mut va = Some(VarAttr {
                is_typedef: false,
                is_static: false,
            });
            // declspec
            let base_type = self.declspec(&mut va);

            let va = va.unwrap();
            if va.is_typedef {
                self.parse_typedef(base_type);
                continue;
            }
            if self.is_function() {
                self.function_definition(base_type, va);
                continue;
            }

            self.global_variable(base_type)
        }

        self.globals.to_vec()
    }

    fn global_variable(&mut self, base_type: Box<Type>) {
        let mut first = true;

        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;

            let type_ = self.declarator(base_type.clone());
            let name = type_.get_name().to_string();
            let obj = Obj::new_gvar(name.to_string(), type_, None);
            self.new_gvar(name.to_string(), obj);
        }
    }

    /// function_definition = declspec declarator "(" ")" "{" compound_stmt*
    fn function_definition(&mut self, base_type: Box<Type>, var_attr: VarAttr) {
        // declarator
        // 声明获取到变量类型，包括变量名
        let type_ = self.declarator(base_type);
        let name = type_.get_name();

        let obj = Obj::new_func(name.to_string(), type_.clone());

        let gvar = self.new_gvar(name.to_string(), obj);

        let mut definition = false;
        let mut params = vec![];
        let mut locals = vec![];
        let mut body = None;

        if !self.consume(";") {
            self.cur_func = gvar.clone(); // 指向当前正值解析的方法
            definition = true;
            // 本地变量清空
            self.locals.clear();
            // 进入新的域
            self.enter_scope();
            self.create_param_lvars(type_.get_params());
            params = self.locals.to_vec();

            self.skip("{");

            // compound_stmt
            body = self.compound_stmt();
            locals = self.locals.to_vec();
            // function.set_function(true, params, self.locals.to_vec(), body);
            // 结束当前域
            self.leave_scope();
        }

        // 把初始化移到这个地方,因为在构建方法的时候里面需要borrow_mut这个obj,如果放前面,会导致rust的RefCell报already mutably borrowed
        let mut function = gvar.as_ref().unwrap().borrow_mut();
        function.set_function(params, locals, body, definition, var_attr.is_static);
    }

    fn push_scope(&mut self, name: String) -> Rc<RefCell<VarScope>> {
        let vs = self.scopes[0].add_var(name);
        return vs.clone();
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

    fn find_typedef(&self, token: &Token) -> Option<Box<Type>> {
        match token {
            Token::Ident { t_str, .. } => {
                let vs = self.find_var(t_str);
                if vs.is_some() {
                    let typedef = vs.as_ref().unwrap().borrow().typedef.clone();
                    if typedef.is_some() {
                        let var = typedef.as_ref().unwrap().clone();
                        return Some(var);
                    }
                }
            }
            _ => {}
        }
        return None;
    }

    /// 创建新的左值
    fn new_lvar(&mut self, base_type: Box<Type>) -> Option<Rc<RefCell<Obj>>> {
        let name = base_type.get_name().to_string();
        let nvar = Rc::new(RefCell::new(Obj::new_lvar(name.to_string(), base_type)));
        self.locals.push(nvar.clone());
        let vs = self.push_scope(name.to_string());
        {
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_var(nvar.clone());
        }
        Some(nvar)
    }

    fn new_gvar(&mut self, name: String, obj: Obj) -> Option<Rc<RefCell<Obj>>> {
        let gvar = Rc::new(RefCell::new(obj));
        self.globals.push(gvar.clone());
        let vs = self.push_scope(name);
        {
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_var(gvar.clone());
        }
        Some(gvar)
    }

    /// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
    ///            | "typedef" | "static"
    ///            | struct_declare | union_declare | typedef_name
    ///            | enum_specifier)+
    /// declarator specifier
    fn declspec(&mut self, attr: &mut Option<VarAttr>) -> Box<Type> {
        // 类型的组合，被表示为例如：LONG+LONG=1<<9
        // 可知long int和int long是等价的。
        const VOID: i32 = 1 << 0;
        const BOOL: i32 = 1 << 2;
        const CHAR: i32 = 1 << 4;
        const SHORT: i32 = 1 << 6;
        const INT: i32 = 1 << 8;
        const LONG: i32 = 1 << 10;
        const OTHER: i32 = 1 << 12;
        const SHORT_INT: i32 = SHORT + INT;
        const LONG_INT: i32 = LONG + INT;
        const LONG_LONG: i32 = LONG + LONG;
        const LONG_LONG_INT: i32 = LONG + LONG + INT;

        let mut type_ = Type::new_int();
        let mut counter = 0; // 记录类型相加的数值

        // 遍历所有类型名的Tok
        loop {
            let (_, token) = self.current();
            if !self.is_typename(token) {
                break;
            }

            if token.equal(KW_TYPEDEF) || token.equal(KW_STATIC) {
                if attr.is_none() {
                    error_token!(
                        token,
                        "storage class specifier is not allowed in this context"
                    );
                }
                let is_t = token.equal(KW_TYPEDEF);
                attr.as_mut().unwrap().is_typedef = is_t;
                attr.as_mut().unwrap().is_static = !is_t;
                if is_t && token.equal(KW_STATIC) {
                    error_token!(token, "typedef and static may not be used together");
                }
                self.next();
                continue;
            }

            // 处理用户定义的类型
            let typ2 = self.find_typedef(token);
            if token.equal(KW_STRUCT)
                || token.equal(KW_UNION)
                || token.equal(KW_ENUM)
                || typ2.is_some()
            {
                if counter > 0 {
                    break;
                }
                if token.equal(KW_STRUCT) {
                    type_ = self.next().struct_declare();
                } else if token.equal(KW_UNION) {
                    type_ = self.next().union_declare();
                } else if token.equal(KW_ENUM) {
                    type_ = self.next().enum_specifier();
                } else {
                    type_ = typ2.unwrap();
                    self.next();
                }
                counter += OTHER;
                continue;
            }

            // 对于出现的类型名加入Counter
            // 每一步的Counter都需要有合法值
            if token.equal(KW_VOID) {
                counter += VOID;
            } else if token.equal(KW_BOOL) {
                counter += BOOL;
            } else if token.equal(KW_CHAR) {
                counter += CHAR;
            } else if token.equal(KW_SHORT) {
                counter += SHORT;
            } else if token.equal(KW_INT) {
                counter += INT;
            } else if token.equal(KW_LONG) {
                counter += LONG;
            } else {
                unreachable!()
            }

            match counter {
                VOID => type_ = Type::new_void(),
                BOOL => type_ = Type::new_bool(),
                CHAR => type_ = Type::new_char(),
                SHORT | SHORT_INT => type_ = Type::new_short(),
                INT => type_ = Type::new_int(),
                LONG | LONG_INT | LONG_LONG | LONG_LONG_INT => type_ = Type::new_long(),
                _ => error_token!(token, "invalid type"),
            }

            self.next();
        }

        return type_;
    }

    /// type_suffix = "(" funcParams | "[" array_dimensions | ε
    fn declarator(&mut self, mut type_: Box<Type>) -> Box<Type> {
        // "*"*
        // 构建所有的（多重）指针
        while self.consume("*") {
            type_ = Type::pointer_to(type_);
        }

        let (start_pos, token) = self.current();
        let mut name = "".to_string();
        // "(" declarator ")"
        if token.equal("(") {
            // 使Tok前进到")"后面的位置
            self.next().declarator(Type::new_int());
            self.skip(")");
            // 获取到括号后面的类型后缀，type_为解析完的类型，pos为分号
            type_ = self.type_suffix(type_);
            // 记录分号位置
            let (end_pos, _) = self.current();
            // 返回最开始
            self.cursor = start_pos;
            // 解析Ty整体作为Base去构造，返回Type的值
            let type_ = self.next().declarator(type_);
            // 等整体标记完,返回分号位置
            self.cursor = end_pos;
            return type_;
        }

        match token {
            Token::Ident { t_str, .. } => {
                name = t_str.to_string();
            }
            _ => {
                error_token!(token, "expected a variable name");
            }
        }

        // type_suffix
        type_ = self.next().type_suffix(type_);
        // ident
        // 变量名 或 函数名
        type_.set_name(name);
        type_
    }

    /// type_suffix = "(" func_params | "[" num "]" type_suffix | ε
    fn type_suffix(&mut self, base_type: Box<Type>) -> Box<Type> {
        let (_, token) = self.current();
        // "(" func_params
        if token.equal("(") {
            return self.next().func_params(base_type);
        }

        // "[" num "]"
        if token.equal("[") {
            return self.next().array_dimensions(base_type);
        }

        return base_type;
    }

    /// array_dimensions = num? "]" typeSuffix
    fn array_dimensions(&mut self, mut base_type: Box<Type>) -> Box<Type> {
        let (_, token) = self.current();
        // "]" 无数组维数的 "[]"
        if token.equal("]") {
            base_type = self.next().type_suffix(base_type);
            return Type::array_of(base_type, -1);
        }

        // 有数组维数的情况
        let size = self.get_number();
        self.next().skip("]");
        base_type = self.type_suffix(base_type);
        Type::array_of(base_type, size as isize)
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

            let mut t = self.declspec(&mut None);
            t = self.declarator(t);

            // 倒序插入
            params.insert(0, *t);
        }
        self.skip(")");

        return Type::func_type(type_, params);
    }

    /// 解析复合语句
    /// compound_stmt =  (typedef | declaration | stmt)* "}"
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
            if self.is_typename(token) {
                let mut va = Some(VarAttr {
                    is_typedef: false,
                    is_static: false,
                });
                let base_type = self.declspec(&mut va);

                // 解析typedef的语句
                if va.unwrap().is_typedef {
                    self.parse_typedef(base_type);
                    continue;
                }
                // declaration
                node = self.declaration(base_type).unwrap();
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
    fn declaration(&mut self, base_type: Box<Type>) -> Option<Node> {
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
            let type_ = self.declarator(base_type.clone());
            if type_.size < 0 {
                let (_, token) = self.current();
                error_token!(token, "variable has incomplete type");
                unreachable!()
            }
            if type_.kind == TypeKind::Void {
                let (_, token) = self.current();
                error_token!(token, "variable declared void");
                unreachable!()
            }

            let nvar = self.new_lvar(type_).unwrap();

            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
            if !token.equal("=") {
                continue;
            }

            // 解析“=”后面的Token
            let node = Node::new_var(nvar, nt);
            let lhs = Box::new(node);
            // 解析递归赋值语句
            let rhs = Box::new(self.next().assign().unwrap());
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
            let mut expr = self.next().expr().unwrap();
            self.skip(";");
            add_type(&mut expr);
            let cur_func = self.cur_func.as_ref().unwrap().clone();
            let cast = Node::new_cast(
                Box::new(expr),
                cur_func
                    .borrow_mut()
                    .get_func_return_type()
                    .as_ref()
                    .unwrap()
                    .clone(),
            );
            let node = Node::new_unary(NodeKind::Return, Box::new(cast), nt);
            return Some(node);
        }

        // 解析if语句
        // "if" "(" expr ")" stmt ("else" stmt)?
        if token.equal(KW_IF) {
            // "(" expr ")"，条件内语句
            self.next().skip("(");
            let cond = Some(Box::new(self.expr().unwrap()));
            self.skip(")");
            // stmt，符合条件后的语句
            let then = Some(Box::new(self.stmt().unwrap()));
            // ("else" stmt)?，不符合条件后的语句
            let mut els = None;
            let (_, token) = self.current();
            if token.equal(KW_ELSE) {
                els = Some(Box::new(self.next().stmt().unwrap()));
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
            self.next().skip("(");

            // 进入for的域
            self.enter_scope();

            //expr_stmt
            let (_, token) = self.current();
            let init;
            if self.is_typename(token) {
                let base_type = self.declspec(&mut None);
                init = Some(Box::new(self.declaration(base_type).unwrap()));
            } else {
                init = Some(Box::new(self.expr_stmt().unwrap()));
            }
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

            // 离开for的域
            self.leave_scope();
            return Some(node);
        }

        // | "while" "(" expr ")" stmt
        if token.equal(KW_WHILE) {
            // "("
            self.next().skip("(");
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
            return self.next().compound_stmt();
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
            let rhs = Box::new(self.next().expr().unwrap());
            return Some(Node::new_binary(NodeKind::Comma, Box::new(node), rhs, nt));
        }

        return Some(node);
    }

    /// 解析赋值
    /// assign = log_or (assign_op assign)?
    fn assign(&mut self) -> Option<Node> {
        // equality
        let node = self.log_or().unwrap();

        // 可能存在递归赋值，如a=b=1
        // ("=" assign)?
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal("=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return Some(Node::new_binary(NodeKind::Assign, Box::new(node), rhs, nt));
        }

        // ("+=" assign)?
        if token.equal("+=") {
            let rhs = Box::new(self.next().assign().unwrap());
            let node = self.add_with_type(Box::new(node), rhs, nt).unwrap();
            return self.assign_op(node);
        }

        // ("-=" assign)?
        if token.equal("-=") {
            let rhs = Box::new(self.next().assign().unwrap());
            let node = self.sub_with_type(Box::new(node), rhs, nt).unwrap();
            return self.assign_op(node);
        }

        // ("*=" assign)?
        if token.equal("*=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::Mul, Box::new(node), rhs, nt));
        }

        // ("/=" assign)?
        if token.equal("/=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::Div, Box::new(node), rhs, nt));
        }

        // ("%=" assign)?
        if token.equal("%=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::Mod, Box::new(node), rhs, nt));
        }

        // ("&=" assign)?
        if token.equal("&=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::BitAnd, Box::new(node), rhs, nt));
        }

        // ("|=" assign)?
        if token.equal("|=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::BitOr, Box::new(node), rhs, nt));
        }

        // ("^=" assign)?
        if token.equal("^=") {
            let rhs = Box::new(self.next().assign().unwrap());
            return self.assign_op(Node::new_binary(NodeKind::BitXor, Box::new(node), rhs, nt));
        }

        Some(node)
    }

    /// log_or = log_and ("||" log_and)*
    fn log_or(&mut self) -> Option<Node> {
        let mut node = self.log_and().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("||") {
                break;
            }
            let rhs = self.next().log_and().unwrap();
            node = Node::new_binary(NodeKind::LogOr, Box::new(node), Box::new(rhs), nt);
        }
        Some(node)
    }

    /// log_and = bit_or ("&&" bit_or)*
    fn log_and(&mut self) -> Option<Node> {
        let mut node = self.bit_or().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("&&") {
                break;
            }
            let rhs = self.next().bit_or().unwrap();
            node = Node::new_binary(NodeKind::LogAnd, Box::new(node), Box::new(rhs), nt);
        }
        Some(node)
    }

    /// bit_or = bit_xor ("|" bit_xor)*
    fn bit_or(&mut self) -> Option<Node> {
        let mut node = self.bit_xor().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("|") {
                break;
            }
            let rhs = self.next().bit_xor().unwrap();
            node = Node::new_binary(NodeKind::BitOr, Box::new(node), Box::new(rhs), nt);
        }
        Some(node)
    }

    /// bit_xor = bit_and ("^" bit_and)*
    fn bit_xor(&mut self) -> Option<Node> {
        let mut node = self.bit_and().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("^") {
                break;
            }
            let rhs = self.next().bit_and().unwrap();
            node = Node::new_binary(NodeKind::BitXor, Box::new(node), Box::new(rhs), nt);
        }
        Some(node)
    }

    /// bit_and = equality ("&" equality)*
    fn bit_and(&mut self) -> Option<Node> {
        let mut node = self.equality().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("&") {
                break;
            }
            let rhs = self.next().equality().unwrap();
            node = Node::new_binary(NodeKind::BitAnd, Box::new(node), Box::new(rhs), nt);
        }
        Some(node)
    }

    /// assign_op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
    /// 转换 A op= B为 TMP = &A, *TMP = *TMP op B
    fn assign_op(&mut self, mut binary: Node) -> Option<Node> {
        // A
        add_type(binary.lhs.as_mut().unwrap());
        // B
        add_type(binary.rhs.as_mut().unwrap());
        let token = &binary.token;

        // TMP
        let var = self
            .new_lvar(Type::pointer_to(
                binary.lhs.as_ref().unwrap().clone().type_.unwrap(),
            ))
            .unwrap();
        // TMP = &A
        let lhs = Node::new_var(var.clone(), token.clone());
        let rhs = Node::new_unary(NodeKind::Addr, binary.lhs.unwrap(), token.clone());
        let expr1 = Node::new_binary(
            NodeKind::Assign,
            Box::new(lhs.clone()),
            Box::new(rhs),
            token.clone(),
        );

        // *TMP = *TMP op B
        let lhs = Node::new_unary(NodeKind::DeRef, Box::new(lhs.clone()), token.clone());
        let rhs = Node::new_binary(
            binary.kind.clone(),
            Box::new(lhs.clone()),
            binary.rhs.unwrap().clone(),
            token.clone(),
        );
        let expr2 = Node::new_binary(
            NodeKind::Assign,
            Box::new(lhs),
            Box::new(rhs),
            token.clone(),
        );

        // TMP = &A, *TMP = *TMP op B
        Some(Node::new_binary(
            NodeKind::Comma,
            Box::new(expr1),
            Box::new(expr2),
            token.clone(),
        ))
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
                let rhs = Box::new(self.next().relational().unwrap());
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
                let rhs = Box::new(self.next().relational().unwrap());
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
                let rhs = Box::new(self.next().add().unwrap());
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
                let rhs = Box::new(self.next().add().unwrap());
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
                let lhs = Box::new(self.next().add().unwrap());
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
                let lhs = Box::new(self.next().add().unwrap());
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
            size = rhs_t.get_base_size() as i64;
        } else {
            n_lhs = lhs;
            n_rhs = rhs;
            size = lhs_t.get_base_size() as i64;
        }

        // ptr + num
        // 指针加法，ptr+1，这里的1不是1个字节，而是1个元素的空间，所以需要 ×size 操作
        let size = Box::new(Node::new_long(size, nt.clone()));
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
            let size: i64 = lhs_t.get_base_size() as i64;
            let size = Box::new(Node::new_long(size, nt.clone()));
            let mut f_rhs = Box::new(Node::new_binary(NodeKind::Mul, rhs, size, nt.clone()));
            add_type(f_rhs.as_mut());
            let mut node = Node::new_binary(NodeKind::Sub, lhs, f_rhs, nt);
            node.set_type(lhs_t);
            return Some(node);
        }

        // ptr - ptr，返回两指针间有多少元素
        if lhs_t.has_base() && rhs_t.has_base() {
            let mut node = Box::new(Node::new_binary(NodeKind::Sub, lhs, rhs, nt.clone()));
            node.set_type(Type::new_long());
            let size: i64 = lhs_t.get_base_size() as i64;
            let size = Box::new(Node::new_num(size, nt.clone()));
            // size.set_type(Type::new_int());
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
                let rhs = Box::new(self.next().mul().unwrap());
                node = self.add_with_type(Box::new(node.unwrap()), rhs, nt);
                continue;
            }

            // "-" mul
            if token.equal("-") {
                let rhs = Box::new(self.next().mul().unwrap());
                node = self.sub_with_type(Box::new(node.unwrap()), rhs, nt);
                continue;
            }

            return node;
        }
    }

    /// 解析乘除
    /// mul = cast ("*" cast | "/" cast | "%" cast)*
    fn mul(&mut self) -> Option<Node> {
        // unary
        let mut node = self.cast();

        // ("*" cast | "/" cast | "%" cast)*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "*" unary
            if token.equal("*") {
                let rhs = Box::new(self.next().cast().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Mul,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // "/" cast
            if token.equal("/") {
                let rhs = Box::new(self.next().cast().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Div,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            // "%" cast
            if token.equal("%") {
                let rhs = Box::new(self.next().cast().unwrap());
                node = Some(Node::new_binary(
                    NodeKind::Mod,
                    Box::new(node.unwrap()),
                    rhs,
                    nt,
                ));
                continue;
            }

            return node;
        }
    }

    /// cast = "(" typeName ")" cast | unary
    fn cast(&mut self) -> Option<Node> {
        let (pos, token) = self.current();
        let next = &self.tokens[pos + 1];
        if token.equal("(") && self.is_typename(next) {
            let typ = self.next().typename();
            self.skip(")");
            let cast = self.cast().unwrap();
            let node = Node::new_cast(Box::new(cast), typ);
            return Some(node);
        }

        self.unary()
    }

    /// 解析一元运算
    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///         | ("++" | "--") unary
    ///         | postfix
    fn unary(&mut self) -> Option<Node> {
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();

        // "+" cast
        if token.equal("+") {
            return self.next().cast();
        }

        // "-" cast
        if token.equal("-") {
            return Some(Node::new_unary(
                NodeKind::Neg,
                Box::new(self.next().cast().unwrap()),
                nt,
            ));
        }

        // "&" cast
        if token.equal("&") {
            return Some(Node::new_unary(
                NodeKind::Addr,
                Box::new(self.next().cast().unwrap()),
                nt,
            ));
        }

        // "*" cast
        if token.equal("*") {
            return Some(Node::new_unary(
                NodeKind::DeRef,
                Box::new(self.next().cast().unwrap()),
                nt,
            ));
        }

        // "!" cast
        if token.equal("!") {
            return Some(Node::new_unary(
                NodeKind::Not,
                Box::new(self.next().cast().unwrap()),
                nt,
            ));
        }

        // "~" cast
        if token.equal("~") {
            return Some(Node::new_unary(
                NodeKind::BitNot,
                Box::new(self.next().cast().unwrap()),
                nt,
            ));
        }

        // 转换 ++i 为 i+=1
        // "++" unary
        if token.equal("++") {
            let unary = self.next().unary().unwrap();
            let node = self
                .add_with_type(
                    Box::new(unary),
                    Box::new(Node::new_num(1, nt.clone())),
                    nt.clone(),
                )
                .unwrap();
            return self.assign_op(node);
        }

        // 转换 --i 为 i-=1
        // "--" unary
        if token.equal("--") {
            let unary = self.next().unary().unwrap();
            let node = self
                .sub_with_type(
                    Box::new(unary),
                    Box::new(Node::new_num(1, nt.clone())),
                    nt.clone(),
                )
                .unwrap();
            return self.assign_op(node);
        }

        // primary
        self.postfix()
    }

    fn enum_specifier(&mut self) -> Box<Type> {
        let typ = Type::new_enum();

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
                error_token!(&tag.unwrap(), "unknown enum type");
                unreachable!()
            }
            let type_ = type_.unwrap();
            if type_.kind != TypeKind::Enum {
                error_token!(&tag.unwrap(), "not an enum tag");
                unreachable!()
            }
            return type_;
        }

        self.skip("{");

        let mut i = 0;
        let mut val = 0;
        loop {
            let (_, token) = self.current();
            if token.equal("}") {
                break;
            }
            if i > 0 {
                self.skip(",");
            }
            i += 1;
            let (_, token) = self.current(); // 重新取,可能被跳过了,
            let name = token.get_name();
            self.next(); // 跳过name

            // 判断是否存在赋值
            let (_, token) = self.current(); // 重新取
            if token.equal("=") {
                val = self.next().get_number();
                self.next(); // 跳过数字
            }

            let vs = self.push_scope(name);
            {
                let mut vsm = vs.as_ref().borrow_mut();
                vsm.set_enum(typ.clone(), val)
            }
            val += 1;
        }

        self.next();

        if tag.is_some() {
            let tag_name = tag.unwrap().get_name();
            self.push_tag_scope(tag_name, typ.clone())
        }

        typ
    }

    /// struct_members = (declspec declarator (","  declarator)* ";")*
    fn struct_members(&mut self, type_: &mut Box<Type>) {
        let mut members = vec![];

        loop {
            let (_, token) = self.current();
            if token.equal("}") {
                break;
            }

            let base_type = self.declspec(&mut None);
            let mut is_first = true;

            while !self.consume(";") {
                if !is_first {
                    self.skip(",");
                }
                is_first = false;

                let type_ = self.declarator(base_type.clone());
                let name = type_.get_name().to_string();
                let member = Member::new(name.to_string(), Some(type_));
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

        let mut offset: isize = 0;

        for member in &mut type_.members {
            let align = member.type_.as_ref().unwrap().align;
            offset = align_to(offset, align);
            member.set_offset(offset);
            offset += member.type_.as_ref().unwrap().size;

            if type_.align < align {
                type_.align = align;
            }
        }
        type_.size = align_to(offset, type_.align);

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
        type_.size = align_to(type_.size, type_.align);

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
        self.next().struct_members(&mut type_);
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

        let lhs_t = lhs.type_.as_ref().unwrap().clone();
        if lhs_t.kind != TypeKind::Struct && lhs_t.kind != TypeKind::Union {
            error_token!(&lhs.as_ref().token, "not a struct nor a union");
        }

        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let mut node = Node::new_unary(NodeKind::Member, lhs, nt.clone());
        node.member = Some(Box::new(self.get_struct_member(&lhs_t, &nt).unwrap()));

        Some(node)
    }

    fn inc_dec(&mut self, mut node: Node, addend: i64) -> Option<Node> {
        add_type(&mut node);
        let token = node.clone().token;
        let typ = node.clone().type_.unwrap();
        let num = Box::new(Node::new_num(addend, token.clone()));
        let num_neg = Box::new(Node::new_num(-addend, token.clone()));
        let add = self
            .add_with_type(Box::new(node), num, token.clone())
            .unwrap();
        let lhs = self.assign_op(add).unwrap();
        let add = self
            .add_with_type(Box::new(lhs), num_neg, token.clone())
            .unwrap();
        let cast = Node::new_cast(Box::new(add), typ);
        Some(cast)
    }
    /// postfix = primary ("[" expr "]" | "." ident)* | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Option<Node> {
        // primary
        let mut node = self.primary().unwrap();

        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // ("[" expr "]")*
            if token.equal("[") {
                // x[y] 等价于 *(x+y)
                let idx = self.next().expr().unwrap();
                self.skip("]");
                let unary = self
                    .add_with_type(Box::new(node), Box::new(idx), nt.clone())
                    .unwrap();
                node = Node::new_unary(NodeKind::DeRef, Box::new(unary), nt);
                continue;
            }

            // "." ident
            if token.equal(".") {
                node = self.next().struct_ref(Box::new(node)).unwrap();
                self.next();
                continue;
            }

            // "->" ident
            if token.equal("->") {
                node = Node::new_unary(NodeKind::DeRef, Box::new(node), nt);
                node = self.next().struct_ref(Box::new(node)).unwrap();
                self.next();
                continue;
            }

            // "++"
            if token.equal("++") {
                node = self.inc_dec(node, 1).unwrap();
                self.next();
                continue;
            }

            // "--"
            if token.equal("--") {
                node = self.inc_dec(node, -1).unwrap();
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
    ///         | "sizeof" "(" typename ")"
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
            let node = self.next().expr();
            self.skip(")");
            return node;
        }

        // "sizeof" "(" typename ")"
        let next_next = &self.tokens[pos + 2];
        if token.equal(KW_SIZEOF) && next.equal("(") && self.is_typename(next_next) {
            self.next().next();
            let typ = self.typename();
            self.skip(")");
            // self.cursor = pos;
            return Some(Node::new_num(typ.size as i64, nt));
        }

        // "sizeof" unary
        if token.equal(KW_SIZEOF) {
            let mut node = self.next().unary();
            add_type(node.as_mut().unwrap());
            let (pos, _) = self.current();
            let nt = self.tokens[pos].clone();
            let size = node.unwrap().get_type().as_ref().unwrap().get_size() as i64;
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
                let vso = self.find_var(&t_str);
                let node;
                if vso.is_some() {
                    let vs = vso.unwrap().clone();
                    let vsb = vs.borrow();
                    let var = &vsb.var;
                    let enum_type = &vsb.enum_type;
                    let enum_val = &vsb.enum_val;
                    if var.is_some() {
                        node = Node::new_var(var.as_ref().unwrap().clone(), nt);
                        self.next();
                        return Some(node);
                    }
                    if enum_type.is_some() {
                        node = Node::new_num(*enum_val, nt);
                        self.next();
                        return Some(node);
                    }
                }
                error_at!(*line_no, *offset, "undefined variable");
                return None;
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

    /// typename = declspec abstract_declarator
    fn typename(&mut self) -> Box<Type> {
        let typ = self.declspec(&mut None);

        return self.abstract_declarator(typ);
    }

    /// abstract_declarator = "*"* ("(" abstract_declarator ")")? type_suffix
    fn abstract_declarator(&mut self, mut base_type: Box<Type>) -> Box<Type> {
        // "*"*
        loop {
            let (_, token) = self.current();
            if !token.equal("*") {
                break;
            }
            base_type = Type::pointer_to(base_type);
            self.next();
        }

        // ("(" abstract_declarator ")")?
        let (start_pos, token) = self.current();
        if token.equal("(") {
            self.next().abstract_declarator(Type::new_int());
            self.skip(")");
            // 获取到括号后面的类型后缀，type_为解析完的类型，pos为分号
            base_type = self.type_suffix(base_type);
            // 记录分号位置
            let (end_pos, _) = self.current();
            // 返回最开始
            self.cursor = start_pos;
            self.next();
            // 解析Ty整体作为Base去构造，返回Type的值
            base_type = self.abstract_declarator(base_type);
            // 等整体标记完,返回分号位置
            self.cursor = end_pos;
            return base_type;
        }

        // type_suffix
        self.type_suffix(base_type)
    }

    // func_call = ident "(" (assign ("," assign)*)? ")"
    fn func_call(&mut self, func_name: String) -> Option<Node> {
        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        self.next().next(); // 1. 跳到(  2.调到参数或者)

        // 查找函数名
        let vso = self.find_var(&func_name);
        if vso.is_none() {
            error_token!(&nt, "implicit declaration of a function");
            unreachable!()
        }
        let vs = vso.unwrap().clone();
        let var = &vs.borrow().var;
        if var.is_none() {
            error_token!(&nt, "not a function");
            unreachable!()
        }

        let t;
        let params;
        // 傻屌rust的编译器一定要我把这下面拆成三行才可以,否则就报错..咱也不懂..也不敢问..回头再研究,能用就行
        let binding = var.as_ref().unwrap().clone();
        let bt = binding.borrow();
        let typ = bt.get_type();
        match typ.kind {
            TypeKind::Func => {
                t = typ;
                params = t.params.to_vec();
            }
            _ => {
                error_token!(&nt, "not a function");
                unreachable!()
            }
        }

        let mut nodes = vec![];

        let mut i = 0;
        loop {
            let (_, token) = self.current();
            if token.equal(")") {
                break;
            }
            if nodes.len() != 0 {
                self.skip(",");
            }
            let mut arg = self.assign().unwrap();
            add_type(&mut arg);

            if i < params.len() {
                let param = &params[i];
                if param.kind == TypeKind::Struct || param.kind == TypeKind::Union {
                    error_token!(&arg.token, "passing struct or union is not supported yet");
                    unreachable!()
                }

                arg = Node::new_cast(Box::new(arg), Box::new(param.clone()));
                i += 1;
            }

            add_type(&mut arg);
            nodes.push(arg);
        }

        self.skip(")");

        let mut node = Node::new(NodeKind::FuncCall, nt);
        node.func_name = func_name;
        node.func_type = Some(t.clone());
        // let t = t.return_type;
        node.type_ = t.clone().return_type;
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
    fn find_var(&self, name: &String) -> Option<Rc<RefCell<VarScope>>> {
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

    /// 判断是否为类型名
    fn is_typename(&self, token: &Token) -> bool {
        let is = token.is_typename();
        if is {
            return true;
        }

        // 查找是否为类型别名
        self.find_typedef(token).is_some()
    }

    fn parse_typedef(&mut self, base_type: Box<Type>) {
        let mut first = true;

        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;
            let typ = self.declarator(base_type.clone());
            let name = typ.get_name().to_string();

            let vs = self.push_scope(name);
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_typedef(typ);
        }
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

    fn get_number(&mut self) -> i64 {
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

    fn new_string_literal(&mut self, str_data: Vec<u8>, base_type: Box<Type>) -> Rc<RefCell<Obj>> {
        let name = format!(".L..{}", self.unique_idx);
        self.unique_idx += 1;
        let obj = Obj::new_gvar(name.to_string(), base_type, Some(str_data));
        // 加入globals
        self.new_gvar(name.to_string(), obj).unwrap()
    }
}
