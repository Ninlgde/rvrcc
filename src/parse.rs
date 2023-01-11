//! AST parser
//!
//! program = (typedef | function_definition* | global_variable)*
//! function_definition = declspec declarator "(" ")" "{" compound_stmt*
//! declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//!            | "typedef" | "static" | "extern"
//!            | "_Alignas" ("(" typename | const_expr ")")
//!            | "signed" | "unsigned"
//!            | struct_declare | union_declare | typedef_name
//!            | enum_specifier)+
//!            | "const" | "volatile" | "auto" | "register" | "restrict"
//!            | "__restrict" | "__restrict__" | "_Noreturn")+
//! enum_specifier = ident? "{" enum_list? "}"
//!                 | ident ("{" enum_list? "}")?
//! enum_list = ident ("=" const_expr)? ("," ident ("=" const_expr)?)* ","?
//! declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type_suffix
//! pointers = ("*" ("const" | "volatile" | "restrict")*)*
//! type_suffix = "(" funcParams | "[" array_dimensions | ε
//! array_dimensions = ("static" | "restrict")* const_expr? "]" typeSuffix
//! func_params = ("void" | param ("," param)* ("," "...")?)? ")"
//! param = declspec declarator
//! compound_stmt = (typedef | declaration | stmt)* "}"
//! declaration = declspec (declarator ("=" initializer)?
//!                         ("," declarator ("=" initializer)?)*)? ";"
//! initializer = string_initializer | array_initializer | struct_initializer
//!             | union_initializer | assign
//! string_initializer = string_literal
//! array_initializer = array_initializer1 | array_initializer2
//! array_initializer1 = "{" initializer ("," initializer)* ","? "}"
//! array_initializer2 = initializer ("," initializer)* ","?
//! struct_initializer = struct_initializer1 | struct_initializer2
//! struct_initializer1 = "{" initializer ("," initializer)* ","? "}"
//! struct_initializer2 = initializer ("," initializer)* ","?
//! union_initializer = "{" initializer "}"
//! stmt = "return" expr? ";"
//!        | "if" "(" expr ")" stmt ("else" stmt)?
//!        | "switch" "(" expr ")" stmt
//!        | "case" const_expr ":" stmt
//!        | "default" ":" stmt
//!        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//!        | "while" "(" expr ")" stmt
//!        | "do" stmt "while" "(" expr ")" ";"
//!        | "goto" ident ";"
//!        | "break" ";"
//!        | ident ":" stmt
//!        | "{" compound_stmt
//!        | expr_stmt
//! expr_stmt = expr? ";"
//! expr = assign ("," expr)?
//! assign = conditional (assign_op assign)?
//! conditional = log_or ("?" expr ":" conditional)?
//! log_or = log_and ("||" log_and)*
//! log_and = bit_or ("&&" bit_or)*
//! bit_or = bit_xor ("|" bit_xor)*
//! bit_xor = bit_and ("^" bit_and)*
//! bit_and = equality ("&" equality)*
//! assign_op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//!          | "<<=" | ">>="
//! equality = relational ("==" relational | "!=" relational)*
//! relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
//! shift = add ("<<" add | ">>" add)*
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
//! postfix "(" typename ")" "{" initializer_list "}"
//!         = ident "(" func_args ")" postfix_tail*
//!         | primary postfix_tail*
//!
//! postfix_tail = "[" expr "]"
//!             | "(" func_args ")"
//!             | "." ident
//!             | "->" ident
//!             | "++"
//!             | "--"
//! primary =  "(" "{" stmt+ "}" ")"
//!         | "(" expr ")"
//!         | "sizeof" "(" typename ")"
//!         | "sizeof" unary
//!         | "_Alignof" "(" typename ")"
//!         | "_Alignof" unary
//!         | ident
//!         | str
//!         | num
//! typename = declspec abstract_declarator
//! abstract_declarator = pointers ("(" abstract_declarator ")")? type_suffix
//! func_call = (assign ("," assign)*)? ")"

use crate::ctype::{add_type, Type, TypeKind, TypeLink};
use crate::initializer::{create_lvar_init, write_gvar_data, InitDesig, Initializer, Relocation};
use crate::keywords::{
    KW_ALIGNAS, KW_ALIGNOF, KW_AUTO, KW_BOOL, KW_BREAK, KW_CASE, KW_CHAR, KW_CONST, KW_CONTINUE,
    KW_DEFAULT, KW_DO, KW_DOUBLE, KW_ELSE, KW_ENUM, KW_EXTERN, KW_FLOAT, KW_FOR, KW_GOTO, KW_IF,
    KW_INT, KW_LONG, KW_NORETURN, KW_REGISTER, KW_RESTRICT, KW_RETURN, KW_SHORT, KW_SIGNED,
    KW_SIZEOF, KW_STATIC, KW_STRUCT, KW_SWITCH, KW_TYPEDEF, KW_UNION, KW_UNSIGNED, KW_VOID,
    KW_VOLATILE, KW_WHILE, KW___RESTRICT, KW___RESTRICT__,
};
use crate::node::{add_with_type, eval, sub_with_type, LabelInfo, Node, NodeKind, NodeLink};
use crate::obj::{Member, Obj, ObjLink, Scope, VarAttr, VarScope};
use crate::token::{Token, TokenVecOps};
use crate::{align_to, error_token, vec_u8_into_i8};
use std::cell::RefCell;
use std::cmp;
use std::rc::Rc;

/// 通过token列表生成ast
pub fn parse(tokens: &Vec<Token>) -> Vec<ObjLink> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

pub(crate) struct Parser<'a> {
    /// 终结符列表
    tokens: &'a Vec<Token>,
    /// 游标
    cursor: usize,
    // 本地变量
    locals: Vec<ObjLink>,
    // 全局变量
    globals: Vec<ObjLink>,
    // 唯一名称idx
    unique_idx: usize,
    // 变量域
    scopes: Vec<Scope>,
    // 当前正在解析的function
    cur_func: Option<ObjLink>,
    /// goto标签列表
    gotos: Vec<Rc<RefCell<LabelInfo>>>,
    /// 标签列表
    labels: Vec<Rc<RefCell<LabelInfo>>>,
    /// 当前的break标签
    brk_label: String,
    /// 当前的continue标签
    ctn_label: String,
    /// 当前的switch节点
    cur_switch: Option<NodeLink>,
}

impl TokenVecOps for Parser<'_> {
    /// 获取所有token的接口
    fn get_tokens(&self) -> &Vec<Token> {
        self.tokens
    }

    /// 获取当前游标的接口
    fn get_cursor(&self) -> usize {
        self.cursor
    }

    /// 游标向后移动的接口
    fn inc_cursor(&mut self, step: usize) {
        self.cursor += step;
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            locals: Vec::new(),
            globals: Vec::new(),
            unique_idx: 0,
            scopes: vec![Scope::new()],
            cur_func: None,
            gotos: Vec::new(),
            labels: Vec::new(),
            brk_label: String::new(),
            ctn_label: String::new(),
            cur_switch: None,
        }
    }

    /// 语法解析入口函数
    /// program = function_definition*
    pub fn parse(&mut self) -> Vec<ObjLink> {
        // "{"
        loop {
            let (_, token) = self.current();
            if token.at_eof() {
                break;
            }
            let mut va = Some(VarAttr::new());
            // declspec
            let base_type = self.declspec(&mut va);

            let va = va.as_ref().unwrap();
            if va.is_typedef {
                self.parse_typedef(base_type);
                continue;
            }
            if self.is_function() {
                self.function_definition(base_type, va);
                continue;
            }

            self.global_variable(base_type, va);
        }

        self.globals.to_vec()
    }

    /// 全局变量
    fn global_variable(&mut self, base_type: TypeLink, var_attr: &VarAttr) {
        let mut first = true;

        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;

            let typ = self.declarator(base_type.clone());
            if typ.borrow().name.is_null() {
                error_token!(&typ.borrow().name_pos, "variable name omitted");
            }
            let name = typ.borrow().get_name().to_string();
            let obj = Obj::new_gvar(name.to_string(), typ);
            let mut var = self.new_gvar(name.to_string(), obj);
            {
                // 包起来防止重复borrow_mut
                let mut var_mut = var.as_mut().unwrap().borrow_mut();
                var_mut.set_definition(!var_attr.is_extern);
                var_mut.set_static(var_attr.is_static);
            }
            // 若有设置，则覆盖全局变量的对齐值
            if var_attr.align != 0 {
                var.as_mut().unwrap().borrow_mut().set_align(var_attr.align);
            }
            let (_, token) = self.current();
            if token.equal("=") {
                self.next().gvar_initializer(var.unwrap());
            }
        }
    }

    /// function_definition = declspec declarator "(" ")" "{" compound_stmt*
    fn function_definition(&mut self, base_type: TypeLink, var_attr: &VarAttr) {
        // declarator
        // 声明获取到变量类型，包括变量名
        let typ = self.declarator(base_type);
        if typ.borrow().name.is_null() {
            error_token!(&typ.borrow().name_pos, "variable name omitted");
        }

        let ct = typ.borrow();
        let name = ct.get_name().clone();

        let obj = Obj::new_func(name.to_string(), typ.clone());

        let gvar = self.new_gvar(name.to_string(), obj);

        let mut definition = false;
        let mut params = vec![];
        let mut locals = vec![];
        let mut body = None;
        let mut va_area = None;

        if !self.consume(";") {
            self.cur_func = gvar.clone(); // 指向当前正值解析的方法
            definition = true;
            // 本地变量清空
            self.locals.clear();
            // 进入新的域
            self.enter_scope();
            self.create_param_lvars(typ.borrow().get_params());
            params = self.locals.to_vec();

            if typ.borrow().is_variadic {
                va_area = self.new_lvar(
                    "__va_area__".to_string(),
                    Type::array_of(Type::new_char(), 64),
                );
            }

            self.skip("{");

            // compound_stmt
            body = self.compound_stmt();
            locals = self.locals.to_vec();
            // function.set_function(true, params, self.locals.to_vec(), body);
            // 结束当前域
            self.leave_scope();
            // 处理goto和标签
            self.resolve_goto_labels();
        }

        // 把初始化移到这个地方,因为在构建方法的时候里面需要borrow_mut这个obj,如果放前面,会导致rust的RefCell报already mutably borrowed
        let mut function = gvar.as_ref().unwrap().borrow_mut();
        function.set_function(
            params,
            va_area,
            locals,
            body,
            definition,
            var_attr.is_static,
        );
    }

    /// 处理goto和标签
    fn resolve_goto_labels(&mut self) {
        for goto in self.gotos.iter() {
            let mut x = goto.borrow_mut();
            for label in self.labels.iter() {
                let y = label.borrow();
                if x.equals(&y) {
                    x.set_unique_label(y.unique_label.to_string())
                }
            }

            if x.unique_label.len() == 0 {
                let t = &x.token;
                error_token!(t, "use of undeclared label");
            }
        }

        self.gotos.clear();
        self.labels.clear();
    }

    /// 讲一个名为`name`的变量域压入当前域中
    fn push_scope(&mut self, name: String) -> Rc<RefCell<VarScope>> {
        let vs = self.scopes[0].add_var(name);
        return vs.clone();
    }

    /// 讲一个名为`name`,类型为`typ`的变量标签压入当前域中
    fn push_tag_scope(&mut self, name: String, typ: TypeLink) {
        self.scopes[0].add_tag(name, typ);
    }

    /// 将形参添加到locals
    fn create_param_lvars(&mut self, params: &Vec<TypeLink>) {
        for param in params.iter() {
            if param.borrow().name.is_null() {
                error_token!(&param.borrow().name_pos, "variable name omitted");
            }
            let name = param.borrow().get_name().to_string();
            self.new_lvar(name, param.clone());
        }
    }

    /// 在域中找token相关的typedef
    fn find_typedef(&self, token: &Token) -> Option<TypeLink> {
        if token.is_ident() {
            let name = token.get_name();
            let vs = self.find_var(&name);
            if let Some(vs) = vs {
                let typedef = &vs.borrow().typedef;
                if typedef.is_some() {
                    let var = typedef.as_ref().unwrap().clone();
                    return Some(var);
                }
            }
        }
        return None;
    }

    /// 创建新的左值
    fn new_lvar(&mut self, name: String, base_type: TypeLink) -> Option<ObjLink> {
        let nvar = Rc::new(RefCell::new(Obj::new_lvar(name.to_string(), base_type)));
        self.locals.push(nvar.clone());
        let vs = self.push_scope(name.to_string());
        {
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_var(nvar.clone());
        }
        Some(nvar)
    }

    /// 创建新的全局变量
    fn new_gvar(&mut self, name: String, obj: Obj) -> Option<ObjLink> {
        let gvar = Rc::new(RefCell::new(obj));
        // 存在定义
        gvar.borrow_mut().set_definition(true);
        gvar.borrow_mut().set_static(true);
        self.globals.insert(0, gvar.clone());
        let vs = self.push_scope(name);
        {
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_var(gvar.clone());
        }
        Some(gvar)
    }

    /// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
    ///            | "typedef" | "static" | "extern"
    ///            | "_Alignas" ("(" typename | const_expr ")")
    ///            | "signed" | "unsigned"
    ///            | struct_declare | union_declare | typedef_name
    ///            | enum_specifier)+
    ///            | "const" | "volatile" | "auto" | "register" | "restrict"
    ///            | "__restrict" | "__restrict__" | "_Noreturn")+
    /// declarator specifier
    fn declspec(&mut self, attr: &mut Option<VarAttr>) -> TypeLink {
        // 类型的组合，被表示为例如：LONG+LONG=1<<9
        // 可知long int和int long是等价的。
        const VOID: i32 = 1 << 0;
        const BOOL: i32 = 1 << 2;
        const CHAR: i32 = 1 << 4;
        const SHORT: i32 = 1 << 6;
        const INT: i32 = 1 << 8;
        const LONG: i32 = 1 << 10;
        const FLOAT: i32 = 1 << 12;
        const DOUBLE: i32 = 1 << 14;
        const OTHER: i32 = 1 << 16;
        const SIGNED: i32 = 1 << 17;
        const UNSIGNED: i32 = 1 << 18;

        let mut typ = Type::new_int();
        let mut counter = 0; // 记录类型相加的数值

        // 遍历所有类型名的Tok
        loop {
            let (_, token) = self.current();
            if !self.is_typename(token) {
                break;
            }

            if token.equal(KW_TYPEDEF) || token.equal(KW_STATIC) || token.equal(KW_EXTERN) {
                if attr.is_none() {
                    error_token!(
                        token,
                        "storage class specifier is not allowed in this context"
                    );
                }
                if token.equal(KW_TYPEDEF) {
                    attr.as_mut().unwrap().is_typedef = true;
                } else if token.equal(KW_STATIC) {
                    attr.as_mut().unwrap().is_static = true;
                } else {
                    attr.as_mut().unwrap().is_extern = true;
                }
                // typedef不应与static/extern一起使用
                let a = attr.as_ref().unwrap();
                if a.is_typedef && (a.is_static || a.is_extern) {
                    error_token!(token, "typedef and static may not be used together");
                }
                self.next();
                continue;
            }

            // 识别这些关键字并忽略
            if self.consume(KW_CONST)
                || self.consume(KW_VOLATILE)
                || self.consume(KW_AUTO)
                || self.consume(KW_REGISTER)
                || self.consume(KW_RESTRICT)
                || self.consume(KW___RESTRICT)
                || self.consume(KW___RESTRICT__)
                || self.consume(KW_NORETURN)
            {
                continue;
            }

            // "_Alignas" ("(" typename | const_expr ")")
            let (_, token) = self.current();
            if token.equal(KW_ALIGNAS) {
                if attr.is_none() {
                    error_token!(token, "_Alignas is not allowed in this context")
                }
                self.next().skip("(");

                let (_, token) = self.current();
                if self.is_typename(token) {
                    attr.as_mut().unwrap().align = self.typename().borrow().align;
                } else {
                    attr.as_mut().unwrap().align = self.const_expr() as isize;
                }
                self.skip(")");
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
                    typ = self.next().struct_declare();
                } else if token.equal(KW_UNION) {
                    typ = self.next().union_declare();
                } else if token.equal(KW_ENUM) {
                    typ = self.next().enum_specifier();
                } else {
                    typ = typ2.unwrap();
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
            } else if token.equal(KW_FLOAT) {
                counter += FLOAT;
            } else if token.equal(KW_DOUBLE) {
                counter += DOUBLE;
            } else if token.equal(KW_SIGNED) {
                counter |= SIGNED;
            } else if token.equal(KW_UNSIGNED) {
                counter |= UNSIGNED;
            } else {
                unreachable!()
            }

            // 判断是否相等
            let eq = |c: i32, vals: Vec<i32>| -> bool {
                for v in vals {
                    if c == v {
                        return true;
                    }
                }
                return false;
            };

            if eq(counter, vec![VOID]) {
                typ = Type::new_void()
            } else if eq(counter, vec![BOOL]) {
                typ = Type::new_bool()
            } else if eq(counter, vec![SIGNED + CHAR]) {
                typ = Type::new_char()
            } else if eq(counter, vec![CHAR, UNSIGNED + CHAR]) {
                typ = Type::new_unsigned_char()
            } else if eq(
                counter,
                vec![SHORT, SHORT + INT, SIGNED + SHORT, SIGNED + SHORT + INT],
            ) {
                typ = Type::new_short()
            } else if eq(counter, vec![UNSIGNED + SHORT, UNSIGNED + SHORT + INT]) {
                typ = Type::new_unsigned_short()
            } else if eq(counter, vec![INT, SIGNED, SIGNED + INT]) {
                typ = Type::new_int()
            } else if eq(counter, vec![UNSIGNED, UNSIGNED + INT]) {
                typ = Type::new_unsigned_int()
            } else if eq(
                counter,
                vec![
                    LONG,
                    LONG + INT,
                    LONG + LONG,
                    LONG + LONG + INT,
                    SIGNED + LONG,
                    SIGNED + LONG + INT,
                    SIGNED + LONG + LONG,
                    SIGNED + LONG + LONG + INT,
                ],
            ) {
                typ = Type::new_long()
            } else if eq(
                counter,
                vec![
                    UNSIGNED + LONG,
                    UNSIGNED + LONG + INT,
                    UNSIGNED + LONG + LONG,
                    UNSIGNED + LONG + LONG + INT,
                ],
            ) {
                typ = Type::new_unsigned_long()
            } else if eq(counter, vec![FLOAT]) {
                typ = Type::new_float()
            } else if eq(counter, vec![DOUBLE, LONG + DOUBLE]) {
                typ = Type::new_double()
            } else {
                error_token!(token, "invalid type")
            }

            self.next();
        }

        return typ;
    }

    /// pointers = ("*" ("const" | "volatile" | "restrict")*)*
    fn pointers(&mut self, mut typ: TypeLink) -> TypeLink {
        // "*"*
        // 构建所有的（多重）指针
        while self.consume("*") {
            typ = Type::pointer_to(typ);

            // 识别这些关键字并忽略
            loop {
                let (_, token) = self.current();
                if token.equal(KW_CONST)
                    || token.equal(KW_VOLATILE)
                    || token.equal(KW_RESTRICT)
                    || token.equal(KW___RESTRICT)
                    || token.equal(KW___RESTRICT__)
                {
                    self.next();
                } else {
                    break;
                }
            }
        }
        return typ;
    }

    /// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type_suffix
    fn declarator(&mut self, mut typ: TypeLink) -> TypeLink {
        // pointers
        typ = self.pointers(typ);

        let (start_pos, token) = self.current();
        // "(" declarator ")"
        if token.equal("(") {
            // 使Tok前进到")"后面的位置
            self.next().declarator(Type::new_int());
            self.skip(")");
            // 获取到括号后面的类型后缀，type_为解析完的类型，pos为分号
            typ = self.type_suffix(typ);
            // 记录分号位置
            let (end_pos, _) = self.current();
            // 返回最开始
            self.cursor = start_pos;
            // 解析Ty整体作为Base去构造，返回Type的值
            let typ = self.next().declarator(typ);
            // 等整体标记完,返回分号位置
            self.cursor = end_pos;
            return typ;
        }

        let mut name = Token::null();
        let name_pos = token.clone();
        if token.is_ident() {
            name = token.clone();
            self.next();
        }
        // type_suffix
        typ = self.type_suffix(typ);
        // ident
        // 变量名 或 函数名
        typ.borrow_mut().set_name(name);
        typ.borrow_mut().name_pos = name_pos;
        typ
    }

    /// type_suffix = "(" funcParams | "[" array_dimensions | ε
    fn type_suffix(&mut self, base_type: TypeLink) -> TypeLink {
        let (_, token) = self.current();
        // "(" func_params
        if token.equal("(") {
            return self.next().func_params(base_type);
        }

        // "[" const_expr "]"
        if token.equal("[") {
            return self.next().array_dimensions(base_type);
        }

        return base_type;
    }

    /// array_dimensions = ("static" | "restrict")* const_expr? "]" typeSuffix
    fn array_dimensions(&mut self, mut base_type: TypeLink) -> TypeLink {
        // ("static" | "restrict")*
        loop {
            let (_, token) = self.current();
            if token.equal(KW_STATIC) || token.equal(KW_RESTRICT) {
                self.next();
            } else {
                break;
            }
        }

        let (_, token) = self.current();
        // "]" 无数组维数的 "[]"
        if token.equal("]") {
            base_type = self.next().type_suffix(base_type);
            return Type::array_of(base_type, -1);
        }

        // 有数组维数的情况
        let size = self.const_expr();
        self.skip("]");
        base_type = self.type_suffix(base_type);
        Type::array_of(base_type, size as isize)
    }

    /// func_params = (param ("," param)* ("," "...")?)? ")"
    /// param = declspec declarator
    fn func_params(&mut self, typ: TypeLink) -> TypeLink {
        // "void"
        let (pos, token) = self.current();
        let next = self.get_token(pos + 1);
        if token.equal(KW_VOID) && next.equal(")") {
            self.next().next();
            return Type::func_type(typ, vec![], false);
        }

        let mut params = vec![];
        let mut is_variadic = false;
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

            // ("," "...")?
            let (_, token) = self.current();
            if token.equal("...") {
                is_variadic = true;
                self.next();
                break;
            }

            let mut t = self.declspec(&mut None);
            t = self.declarator(t);
            let tc = t.clone();
            let tb = tc.borrow();
            let name = tb.name.clone();
            // T类型的数组被转换为T*
            if tb.kind == TypeKind::Array {
                t = Type::pointer_to(tb.base.as_ref().unwrap().clone());
                t.borrow_mut().set_name(name);
            } else if tb.kind == TypeKind::Func {
                t = Type::pointer_to(tc.clone());
                t.borrow_mut().set_name(name);
            }

            // 倒序插入
            params.insert(0, t);
        }
        self.skip(")");

        // 设置空参函数调用为可变的
        if params.len() == 0 {
            is_variadic = true;
        }

        return Type::func_type(typ, params, is_variadic);
    }

    /// 解析复合语句
    /// compound_stmt =  (typedef | declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Option<NodeLink> {
        let mut nodes = vec![];
        let (pos, _) = self.current();
        let nt = &self.tokens[pos];

        // 进入新的域
        self.enter_scope();

        // 逐句解析,并压入nodes
        // (declaration | stmt)* "}"
        loop {
            let (pos, token) = self.current();
            if token.equal("}") {
                break;
            }
            let mut node;
            if self.is_typename(token) && !self.get_token(pos + 1).equal(":") {
                let mut attr = Some(VarAttr::new());
                let base_type = self.declspec(&mut attr);

                // 解析typedef的语句
                let va = attr.as_ref().unwrap();
                if va.is_typedef {
                    self.parse_typedef(base_type);
                    continue;
                }

                // 解析函数
                if self.is_function() {
                    self.function_definition(base_type, va);
                    continue;
                }

                // 解析外部全局变量
                if va.is_extern {
                    self.global_variable(base_type, va);
                    continue;
                }

                // declaration 解析变量声明语句
                node = self.declaration(base_type, &mut attr).unwrap();
            } else {
                // stmt
                node = self.stmt().unwrap();
            }
            add_type(&mut node);
            nodes.push(node);
        }

        let node = Node::new_block(NodeKind::Block, nodes, nt.clone());
        self.next();

        // 结束当前的域
        self.leave_scope();

        Some(node)
    }

    /// declaration = declspec (declarator ("=" initializer)?
    ///                        ("," declarator ("=" initializer)?)*)? ";"
    fn declaration(&mut self, base_type: TypeLink, attr: &mut Option<VarAttr>) -> Option<NodeLink> {
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
            let typ = self.declarator(base_type.clone());
            if typ.borrow().kind == TypeKind::Void {
                let (_, token) = self.current();
                error_token!(token, "variable declared void");
                unreachable!()
            }
            if typ.borrow().name.is_null() {
                error_token!(&typ.borrow().name_pos, "variable name omitted");
            }

            if attr.is_some() && attr.as_ref().unwrap().is_static {
                let gvar = self.new_anon_gvar(typ.clone());
                let name = typ.borrow().get_name().to_string();
                let vs = self.push_scope(name);
                {
                    let mut vsm = vs.as_ref().borrow_mut();
                    vsm.set_var(gvar.clone());
                }
                let (_, token) = self.current();
                if token.equal("=") {
                    self.next().gvar_initializer(gvar);
                }
                continue;
            }

            let name = typ.borrow().get_name().to_string();
            let nvar = self.new_lvar(name, typ).unwrap();
            // 读取是否存在变量的对齐值
            if attr.is_some() && attr.as_ref().unwrap().align != 0 {
                nvar.borrow_mut().set_align(attr.as_ref().unwrap().align);
            }

            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
            if token.equal("=") {
                // 解析变量的初始化器
                let expr = self.next().lvar_initializer(nvar.clone());
                // 存放在表达式语句中
                let node = Node::new_unary(NodeKind::ExprStmt, expr.unwrap(), nt);
                nodes.push(node);
                continue;
            }

            let var = nvar.clone();
            let var = var.borrow();
            let t = var.get_type().borrow();
            if t.size < 0 {
                error_token!(&t.name, "o variable has incomplete type");
            }
            if t.kind == TypeKind::Void {
                error_token!(&t.name, "o variable declared void");
            }
        }

        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let node = Node::new_block(NodeKind::Block, nodes, nt);
        self.next();

        Some(node)
    }

    /// union_initializer = "{" initializer "}"
    fn union_initializer(&mut self, init: &mut Box<Initializer>) {
        // 联合体只接受第一个成员用来初始化
        let (_, token) = self.current();
        if token.equal("{") {
            // 存在括号的情况
            self.next().initializer0(&mut init.children[0]);
            self.consume(",");
            self.skip("}");
        } else {
            // 不存在括号的情况
            self.initializer0(&mut init.children[0]);
        }
    }

    /// struct_initializer1 = "{" initializer ("," initializer)* ","? "}"
    fn struct_initializer1(&mut self, init: &mut Box<Initializer>) {
        let typ = init.typ.as_ref().unwrap().clone();
        let t = typ.borrow();
        self.skip("{");

        // 项数
        let mut i = 0;
        while !self.consume_end() {
            if i > 0 {
                self.skip(",");
            }

            if i < t.members.len() as usize {
                // 正常解析元素
                self.initializer0(&mut init.children[i]);
            } else {
                // 跳过多余的元素
                self.skip_excess_element();
            }
            i += 1;
        }
    }

    /// struct_initializer2 = initializer ("," initializer)* ","?
    fn struct_initializer2(&mut self, init: &mut Box<Initializer>) {
        let typ = init.typ.as_ref().unwrap().clone();
        let t = typ.borrow();

        // 项数
        let mut i = 0;
        loop {
            if i >= t.members.len() as usize || self.is_end() {
                break;
            }
            if i > 0 {
                self.skip(",");
            }
            self.initializer0(&mut init.children[i]);
            i += 1;
        }
    }

    /// string_initializer = string_literal
    fn string_initializer(&mut self, init: &mut Box<Initializer>) {
        let t = init.typ.as_ref().unwrap().clone();
        let t = t.borrow();

        let (_, token) = self.current();
        let (chars, token_type) = token.get_string();
        // 如果是可调整的，就构造一个包含数组的初始化器
        // 字符串字面量在词法解析部分已经增加了'\0'
        if init.is_flexible {
            let new_type =
                Type::array_of(t.base.as_ref().unwrap().clone(), token_type.borrow().len);
            *init = Initializer::new(new_type, false);
        }
        let t = init.typ.as_ref().unwrap().clone();
        let t = t.borrow();
        let (_, token) = self.current();
        let mut len = token_type.borrow().len;
        len = cmp::min(t.len, len);
        for i in 0..len as usize {
            let mut child = &mut init.children[i];
            child.expr = Some(Node::new_num(chars[i as usize] as i64, token.clone()));
        }
        self.next();
    }

    /// 跳过多余的元素
    fn skip_excess_element(&mut self) {
        let (_, token) = self.current();
        if token.equal("{") {
            self.next().skip_excess_element();
            self.skip("}");
        }

        self.assign();
    }

    /// 计算数组初始化元素个数
    fn count_array_init_elements(&mut self, typ: TypeLink) -> isize {
        let (start_pos, _) = self.current();
        let mut dummy = Initializer::new(typ.borrow().base.as_ref().unwrap().clone(), false);

        // 项数
        let mut count = 0;
        // 遍历所有匹配的项
        while !self.consume_end() {
            if count > 0 {
                self.skip(",");
            }
            self.initializer0(&mut dummy);
            count += 1;
        }

        // cursor 回档
        self.cursor = start_pos;
        return count;
    }

    /// array_initializer1 = "{" initializer ("," initializer)* ","? "}"
    fn array_initializer1(&mut self, init: &mut Box<Initializer>) {
        let typ = init.typ.as_ref().unwrap().clone();
        let t = typ.borrow();
        self.skip("{");
        // 如果数组是可调整的，那么就计算数组的元素数，然后进行初始化器的构造
        if init.is_flexible {
            let len = self.count_array_init_elements(typ.clone());
            // 在这里Ty也被重新构造为了数组
            let new_type = Type::array_of(t.base.as_ref().unwrap().clone(), len);
            *init = Initializer::new(new_type, false);
        }
        let typ = init.typ.as_ref().unwrap().clone(); // 可能被替换了,重新取下
        let t = typ.borrow();

        // 遍历数组
        let mut i = 0;
        while !self.consume_end() {
            if i > 0 {
                self.skip(",");
            }
            if i < t.len as usize {
                // 正常解析元素
                self.initializer0(&mut init.children[i]);
            } else {
                // 跳过多余的元素
                self.skip_excess_element();
            }
            i += 1;
        }
    }

    /// array_initializer2 = initializer ("," initializer)* ","?
    fn array_initializer2(&mut self, init: &mut Box<Initializer>) {
        let typ = init.typ.as_ref().unwrap().clone();
        let t = typ.borrow();
        // 如果数组是可调整的，那么就计算数组的元素数，然后进行初始化器的构造
        if init.is_flexible {
            let len = self.count_array_init_elements(typ.clone());
            // 在这里Ty也被重新构造为了数组
            let new_type = Type::array_of(t.base.as_ref().unwrap().clone(), len);
            *init = Initializer::new(new_type, false);
        }
        let typ = init.typ.as_ref().unwrap().clone(); // 可能被替换了,重新取下
        let t = typ.borrow();

        // 遍历数组
        let mut i = 0;
        loop {
            if i >= t.len as usize || self.is_end() {
                break;
            }
            if i > 0 {
                self.skip(",");
            }
            self.initializer0(&mut init.children[i]);
            i += 1;
        }
    }

    /// initializer = string_initializer | array_initializer | string_initializer
    ///             | union_initializer | assign
    fn initializer0(&mut self, init: &mut Box<Initializer>) {
        // string_initializer
        let t = init.typ.as_ref().unwrap().clone();
        let t = t.borrow();
        let (_, token) = self.current();
        if t.kind == TypeKind::Array && token.is_string() {
            self.string_initializer(init);
            return;
        }

        // array_initializer
        if t.kind == TypeKind::Array {
            if token.equal("{") {
                self.array_initializer1(init);
            } else {
                self.array_initializer2(init);
            }
            return;
        }

        // struct_initializer
        if t.kind == TypeKind::Struct {
            // 匹配使用其他结构体来赋值，其他结构体需要先被解析过
            // 存在括号的情况
            if token.equal("{") {
                self.struct_initializer1(init);
                return;
            }
            // 不存在括号的情况
            let (start_pos, _) = self.current();
            let mut expr = self.assign().unwrap();
            add_type(&mut expr);
            if expr.typ.as_ref().unwrap().borrow().kind == TypeKind::Struct {
                init.expr = Some(expr);
                return;
            }
            // 如果通过assign 读到的不是struct,则回档,使用struct_initializer2来读
            self.cursor = start_pos;
            self.struct_initializer2(init);
            return;
        }

        // union_initializer
        if t.kind == TypeKind::Union {
            self.union_initializer(init);
            return;
        }

        if token.equal("{") {
            self.next().initializer0(init);
            self.skip("}");
            return;
        }

        // assign
        // 为节点存储对应的表达式
        init.expr = self.assign()
    }

    /// 初始化器
    fn initializer(&mut self, var: ObjLink) -> (Option<Box<Initializer>>, TypeLink) {
        let binding = var.borrow();
        let typ = binding.get_type();
        // 新建一个解析了类型的初始化器
        let mut init = Initializer::new(typ.clone(), true);
        // 解析需要赋值到Init中
        self.initializer0(&mut init);

        let t = typ.borrow();
        if (t.kind == TypeKind::Struct || t.kind == TypeKind::Union) && t.is_flexible {
            // 复制结构体类型
            let new_type = Type::copy_struct_type(typ);
            let mut t = new_type.borrow_mut();
            // 取最后一个成员
            let last = t.members.last_mut().unwrap();
            let lt = last.typ.as_mut().unwrap();
            let idx = last.idx;
            // 灵活数组类型替换为实际的数组类型
            *lt = init.children[idx].typ.as_ref().unwrap().clone();
            let size = lt.borrow().size;
            // 增加结构体的类型大小
            t.size += size;

            return (Some(init), new_type.clone());
        }

        let new_type = init.typ.as_ref().unwrap().clone();
        (Some(init), new_type)
    }

    /// 局部变量初始化器
    fn lvar_initializer(&mut self, var: ObjLink) -> Option<NodeLink> {
        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();

        // 获取初始化器，将值与数据结构一一对应
        let (init, new_type) = self.initializer(var.clone());
        var.borrow_mut().set_type(new_type);
        // 指派初始化
        let id = InitDesig::new_with_var(var.clone());
        // 我们首先为所有元素赋0，然后有指定值的再进行赋值
        let mut lhs = Node::new(NodeKind::MemZero, nt.clone());
        lhs.var = Some(var.clone());
        // 创建局部变量的初始化
        let binding = var.borrow();
        let typ = binding.get_type();
        let rhs = create_lvar_init(init.unwrap(), typ.clone(), id, nt.clone()).unwrap();
        // 左部为全部清零，右部为需要赋值的部分
        return Some(Node::new_binary(NodeKind::Comma, lhs, rhs, nt.clone()));
    }

    /// 全局变量在编译时需计算出初始化的值，然后写入.data段。
    fn gvar_initializer(&mut self, var: ObjLink) {
        // 获取到初始化器
        let (init, new_type) = self.initializer(var.clone());
        let mut binding = var.borrow_mut();
        binding.set_type(new_type);

        // 写入计算过后的数据
        let typ = binding.get_type();
        let size = typ.borrow().size as usize;
        let mut buf = Vec::with_capacity(size);
        for _ in 0..size {
            buf.push(0);
        }
        {
            let head = Relocation::head();
            write_gvar_data(head, init.unwrap(), &typ, &mut buf, 0);
            binding.set_init_data(buf);
            unsafe { binding.set_relocation((*head).next) }
        }
    }

    /// 解析语句
    /// stmt = "return" expr? ";"
    ///        | "if" "(" expr ")" stmt ("else" stmt)?
    ///        | "switch" "(" expr ")" stmt
    ///        | "case" const_expr ":" stmt
    ///        | "default" ":" stmt
    ///        | "for" "(" exprStmt expr? ";" expr? ")" stmt
    ///        | "while" "(" expr ")" stmt
    ///        | "do" stmt "while" "(" expr ")" ";"
    ///        | "goto" ident ";"
    ///        | "break" ";"
    ///        | "continue" ";"
    ///        | ident ":" stmt
    ///        | "{" compound_stmt
    ///        | expr_stmt
    fn stmt(&mut self) -> Option<NodeLink> {
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        // "return" expr ";"
        if token.equal(KW_RETURN) {
            // 空返回语句
            if self.next().consume(";") {
                return Some(Node::new(NodeKind::Return, nt));
            }
            // 不是空语句,恢复cursor
            self.cursor = pos;

            let mut expr = self.next().expr().unwrap();
            self.skip(";");
            add_type(&mut expr);
            let cur_func = self.cur_func.as_ref().unwrap().clone();
            let cast = Node::new_cast(
                expr,
                cur_func
                    .borrow_mut()
                    .get_func_return_type()
                    .as_ref()
                    .unwrap()
                    .clone(),
            );
            let node = Node::new_unary(NodeKind::Return, cast, nt);
            return Some(node);
        }

        // "switch" "(" expr ")" stmt
        if token.equal(KW_SWITCH) {
            let mut node = Node::new(NodeKind::Switch, nt);
            self.next().skip("(");
            node.cond = Some(self.expr().unwrap());
            self.skip(")");

            // 存储此前break标签的名称
            let brk_label = self.brk_label.to_string();
            // 设置break标签的名称
            self.brk_label = self.new_unique_name();
            node.break_label = Some(self.brk_label.to_string());

            let sw = self.cur_switch.take();

            self.cur_switch = Some(node);

            // 进入解析各个case
            // stmt
            let then = self.stmt().unwrap();
            node = self.cur_switch.take().unwrap();
            node.then = Some(then);

            self.cur_switch = sw;
            // 恢复此前break标签的名称
            self.brk_label = brk_label;
            return Some(node);
        }

        // "case" const_expr ":" stmt
        if token.equal(KW_CASE) {
            if self.cur_switch.is_none() {
                let (_, token) = self.current();
                error_token!(token, "stray case");
                unreachable!()
            }

            let mut node = Node::new(NodeKind::Case, nt.clone());
            // case后面的数值
            let val = self.next().const_expr();
            self.skip(":");
            node.continue_label = Some(self.new_unique_name());
            // case中的语句
            node.lhs = Some(self.stmt().unwrap());
            // case对应的数值
            node.val = val;

            // 将Nd存入CurrentSwitch的CaseNext
            self.cur_switch
                .as_mut()
                .unwrap()
                .case_next
                .insert(0, node.clone());

            return Some(node);
        }

        if token.equal(KW_DEFAULT) {
            if self.cur_switch.is_none() {
                let (_, token) = self.current();
                error_token!(token, "stray default");
                unreachable!()
            }

            let mut node = Node::new(NodeKind::Case, nt.clone());
            self.next().skip(":");
            node.continue_label = Some(self.new_unique_name());
            node.lhs = Some(self.stmt().unwrap());

            // 存入CurrentSwitch->DefaultCase的默认标签
            self.cur_switch.as_mut().unwrap().default_case = Some(node.clone());

            return Some(node);
        }

        // 解析if语句
        // "if" "(" expr ")" stmt ("else" stmt)?
        if token.equal(KW_IF) {
            // "(" expr ")"，条件内语句
            self.next().skip("(");
            let cond = Some(self.expr().unwrap());
            self.skip(")");
            // stmt，符合条件后的语句
            let then = Some(self.stmt().unwrap());
            // ("else" stmt)?，不符合条件后的语句
            let mut els = None;
            let (_, token) = self.current();
            if token.equal(KW_ELSE) {
                els = Some(self.next().stmt().unwrap());
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

            // 存储此前break和continue标签的名称
            let brk_label = self.brk_label.to_string();
            let ctn_label = self.ctn_label.to_string();
            // 设置break和continue标签的名称
            self.brk_label = self.new_unique_name();
            self.ctn_label = self.new_unique_name();

            //expr_stmt
            let (_, token) = self.current();
            let init;
            if self.is_typename(token) {
                let base_type = self.declspec(&mut None);
                init = Some(self.declaration(base_type, &mut None).unwrap());
            } else {
                init = Some(self.expr_stmt().unwrap());
            }
            // expr?
            let mut cond = None;
            let (_, token) = self.current();
            if !token.equal(";") {
                cond = Some(self.expr().unwrap());
            }
            // ";"
            self.skip(";");
            // expr?
            let mut inc = None;
            let (_, token) = self.current();
            if !token.equal(")") {
                inc = Some(self.expr().unwrap());
            }
            // ")"
            self.skip(")");
            // stmt
            let then = Some(self.stmt().unwrap());

            let mut node = Node::new(NodeKind::For, nt);
            node.init = init;
            node.inc = inc;
            node.cond = cond;
            node.then = then;
            node.break_label = Some(self.brk_label.to_string());
            node.continue_label = Some(self.ctn_label.to_string());

            // 离开for的域
            self.leave_scope();
            // 恢复此前的break和continue标签
            self.brk_label = brk_label;
            self.ctn_label = ctn_label;
            return Some(node);
        }

        // | "while" "(" expr ")" stmt
        if token.equal(KW_WHILE) {
            // "("
            self.next().skip("(");
            // expr
            let cond = Some(self.expr().unwrap());
            // ")"
            self.skip(")");

            // 存储此前break和continue标签的名称
            let brk_label = self.brk_label.to_string();
            let ctn_label = self.ctn_label.to_string();
            // 设置break和continue标签的名称
            self.brk_label = self.new_unique_name();
            self.ctn_label = self.new_unique_name();

            // stmt
            let then = Some(self.stmt().unwrap());

            let mut node = Node::new(NodeKind::For, nt);
            node.cond = cond;
            node.then = then;
            node.break_label = Some(self.brk_label.to_string());
            node.continue_label = Some(self.ctn_label.to_string());

            // 恢复此前的break和continue标签
            self.brk_label = brk_label;
            self.ctn_label = ctn_label;
            return Some(node);
        }

        // "do" stmt "while" "(" expr ")" ";"
        if token.equal(KW_DO) {
            let mut node = Node::new(NodeKind::Do, nt);

            // 存储此前break和continue标签的名称
            let brk_label = self.brk_label.to_string();
            let ctn_label = self.ctn_label.to_string();
            // 设置break和continue标签的名称
            self.brk_label = self.new_unique_name();
            self.ctn_label = self.new_unique_name();
            node.break_label = Some(self.brk_label.to_string());
            node.continue_label = Some(self.ctn_label.to_string());

            // stmt
            // do代码块内的语句
            node.then = Some(self.next().stmt().unwrap());

            // 恢复此前的break和continue标签
            self.brk_label = brk_label;
            self.ctn_label = ctn_label;

            // "while" "(" expr ")" ";"
            self.skip(KW_WHILE);
            self.skip("(");
            // expr
            // while使用的条件表达式
            node.cond = Some(self.expr().unwrap());
            self.skip(")");
            self.skip(";");
            return Some(node);
        }

        // "goto" ident ";"
        if token.equal(KW_GOTO) {
            let mut node = Node::new(NodeKind::Goto, nt.clone());
            let label = self.get_token(pos + 1).get_name().to_string();
            let label = LabelInfo::new_goto(label, nt.clone());
            node.label_info = Some(label.clone());
            self.gotos.insert(0, label);
            self.next().next().skip(";");
            return Some(node);
        }

        if token.equal(KW_BREAK) {
            if self.brk_label.len() == 0 {
                error_token!(&nt.clone(), "stray break");
            }
            let label = self.brk_label.to_string();
            let label = LabelInfo::new_break(label, nt.clone());
            // 跳转到break标签的位置
            let mut node = Node::new(NodeKind::Goto, nt.clone());
            node.label_info = Some(label.clone());
            self.next().skip(";");
            return Some(node);
        }

        if token.equal(KW_CONTINUE) {
            if self.brk_label.len() == 0 {
                error_token!(&nt.clone(), "stray continue");
            }
            let label = self.ctn_label.to_string();
            let label = LabelInfo::new_break(label, nt.clone());
            // 跳转到continue标签的位置
            let mut node = Node::new(NodeKind::Goto, nt.clone());
            node.label_info = Some(label.clone());
            self.next().skip(";");
            return Some(node);
        }

        // ident ":" stmt
        if token.is_ident() && self.get_token(pos + 1).equal(":") {
            let mut node = Node::new(NodeKind::Label, nt.clone());
            let label = nt.get_name().to_string();
            let unique_label = self.new_unique_name();
            let label = LabelInfo::new(label, unique_label, nt.clone());
            node.label_info = Some(label.clone());
            self.labels.insert(0, label);
            node.lhs = Some(self.next().next().stmt().unwrap());

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
    fn expr_stmt(&mut self) -> Option<NodeLink> {
        // ;
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal(";") {
            self.next();
            return Some(Node::new_block(NodeKind::Block, vec![], nt));
        }

        // expr ";"
        let node = Node::new_unary(NodeKind::ExprStmt, self.expr().unwrap(), nt);
        self.skip(";");

        Some(node)
    }

    /// 解析常量表达式
    pub(crate) fn const_expr(&mut self) -> i64 {
        // 进行常量表达式的构造
        let mut node = self.conditional().unwrap();

        return eval(&mut node);
    }

    /// 解析表达式
    /// expr = assign ("," expr)?
    fn expr(&mut self) -> Option<NodeLink> {
        // assign
        let node = self.assign().unwrap();

        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal(",") {
            let rhs = self.next().expr().unwrap();
            return Some(Node::new_binary(NodeKind::Comma, node, rhs, nt));
        }

        return Some(node);
    }

    /// 转换 A op= B为 TMP = &A, *TMP = *TMP op B
    fn to_assign(&mut self, mut binary: NodeLink) -> Option<NodeLink> {
        // A
        add_type(binary.lhs.as_mut().unwrap());
        // B
        add_type(binary.rhs.as_mut().unwrap());
        let token = &binary.token;

        // TMP
        let var = self
            .new_lvar(
                "".to_string(),
                Type::pointer_to(binary.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone()),
            )
            .unwrap();
        // TMP = &A
        let lhs = Node::new_var(var.clone(), token.clone());
        let rhs = Node::new_unary(NodeKind::Addr, binary.lhs.unwrap(), token.clone());
        let expr1 = Node::new_binary(NodeKind::Assign, lhs.clone(), rhs, token.clone());

        // *TMP = *TMP op B
        let lhs = Node::new_unary(NodeKind::DeRef, lhs.clone(), token.clone());
        let rhs = Node::new_binary(
            binary.kind.clone(),
            lhs.clone(),
            binary.rhs.take().unwrap(),
            token.clone(),
        );
        let expr2 = Node::new_binary(NodeKind::Assign, lhs, rhs, token.clone());

        // TMP = &A, *TMP = *TMP op B
        Some(Node::new_binary(
            NodeKind::Comma,
            expr1,
            expr2,
            token.clone(),
        ))
    }

    /// 解析赋值
    /// assign = conditional (assign_op assign)?
    /// assign_op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
    ///           | "<<=" | ">>="
    fn assign(&mut self) -> Option<NodeLink> {
        // equality
        let node = self.conditional().unwrap();

        // 可能存在递归赋值，如a=b=1
        // ("=" assign)?
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        if token.equal("=") {
            let rhs = self.next().assign().unwrap();
            return Some(Node::new_binary(NodeKind::Assign, node, rhs, nt));
        }

        // ("+=" assign)?
        if token.equal("+=") {
            let rhs = self.next().assign().unwrap();
            let node = add_with_type(node, rhs, nt).unwrap();
            return self.to_assign(node);
        }

        // ("-=" assign)?
        if token.equal("-=") {
            let rhs = self.next().assign().unwrap();
            let node = sub_with_type(node, rhs, nt).unwrap();
            return self.to_assign(node);
        }

        // ("*=" assign)?
        if token.equal("*=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::Mul, node, rhs, nt));
        }

        // ("/=" assign)?
        if token.equal("/=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::Div, node, rhs, nt));
        }

        // ("%=" assign)?
        if token.equal("%=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::Mod, node, rhs, nt));
        }

        // ("&=" assign)?
        if token.equal("&=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::BitAnd, node, rhs, nt));
        }

        // ("|=" assign)?
        if token.equal("|=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::BitOr, node, rhs, nt));
        }

        // ("^=" assign)?
        if token.equal("^=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::BitXor, node, rhs, nt));
        }

        // ("<<=" assign)?
        if token.equal("<<=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::Shl, node, rhs, nt));
        }

        // (">>=" assign)?
        if token.equal(">>=") {
            let rhs = self.next().assign().unwrap();
            return self.to_assign(Node::new_binary(NodeKind::Shr, node, rhs, nt));
        }

        Some(node)
    }

    /// conditional = log_or ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Option<NodeLink> {
        // log_or
        let cond = self.log_or().unwrap();

        let (_, token) = self.current();
        // "?"
        if !token.equal("?") {
            return Some(cond);
        }

        // expr ":" conditional
        let mut node = Node::new(NodeKind::Cond, token.clone());
        node.cond = Some(cond);
        // expr ":"
        node.then = self.next().expr();
        self.skip(":");
        // conditional，这里不能被解析为赋值式
        node.els = self.conditional();

        Some(node)
    }

    /// log_or = log_and ("||" log_and)*
    fn log_or(&mut self) -> Option<NodeLink> {
        let mut node = self.log_and().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("||") {
                break;
            }
            let rhs = self.next().log_and().unwrap();
            node = Node::new_binary(NodeKind::LogOr, node, rhs, nt);
        }
        Some(node)
    }

    /// log_and = bit_or ("&&" bit_or)*
    fn log_and(&mut self) -> Option<NodeLink> {
        let mut node = self.bit_or().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("&&") {
                break;
            }
            let rhs = self.next().bit_or().unwrap();
            node = Node::new_binary(NodeKind::LogAnd, node, rhs, nt);
        }
        Some(node)
    }

    /// bit_or = bit_xor ("|" bit_xor)*
    fn bit_or(&mut self) -> Option<NodeLink> {
        let mut node = self.bit_xor().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("|") {
                break;
            }
            let rhs = self.next().bit_xor().unwrap();
            node = Node::new_binary(NodeKind::BitOr, node, rhs, nt);
        }
        Some(node)
    }

    /// bit_xor = bit_and ("^" bit_and)*
    fn bit_xor(&mut self) -> Option<NodeLink> {
        let mut node = self.bit_and().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("^") {
                break;
            }
            let rhs = self.next().bit_and().unwrap();
            node = Node::new_binary(NodeKind::BitXor, node, rhs, nt);
        }
        Some(node)
    }

    /// bit_and = equality ("&" equality)*
    fn bit_and(&mut self) -> Option<NodeLink> {
        let mut node = self.equality().unwrap();
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            if !token.equal("&") {
                break;
            }
            let rhs = self.next().equality().unwrap();
            node = Node::new_binary(NodeKind::BitAnd, node, rhs, nt);
        }
        Some(node)
    }

    /// 解析相等性
    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Option<NodeLink> {
        // relational
        let mut node = self.relational();

        // ("==" relational | "!=" relational)*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "==" relational
            if token.equal("==") {
                let rhs = self.next().relational().unwrap();
                node = Some(Node::new_binary(NodeKind::Eq, node.unwrap(), rhs, nt));
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                let rhs = self.next().relational().unwrap();
                node = Some(Node::new_binary(NodeKind::Ne, node.unwrap(), rhs, nt));
                continue;
            }

            return node;
        }
    }

    /// 解析比较关系
    /// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Option<NodeLink> {
        // shift
        let mut node = self.shift();

        // ("<" shift | "<=" shift | ">" shift | ">=" shift)*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "<" shift
            if token.equal("<") {
                let rhs = self.next().shift().unwrap();
                node = Some(Node::new_binary(NodeKind::Lt, node.unwrap(), rhs, nt));
                continue;
            }

            // "<=" shift
            if token.equal("<=") {
                let rhs = self.next().shift().unwrap();
                node = Some(Node::new_binary(NodeKind::Le, node.unwrap(), rhs, nt));
                continue;
            }

            // ">" shift
            // X>Y等价于Y<X
            if token.equal(">") {
                let lhs = self.next().shift().unwrap();
                node = Some(Node::new_binary(NodeKind::Lt, lhs, node.unwrap(), nt));
                continue;
            }

            // ">=" shift
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                let lhs = self.next().shift().unwrap();
                node = Some(Node::new_binary(NodeKind::Le, lhs, node.unwrap(), nt));
                continue;
            }

            return node;
        }
    }

    /// shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Option<NodeLink> {
        // shift
        let mut node = self.add();

        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "<<" add
            if token.equal("<<") {
                let rhs = self.next().add().unwrap();
                node = Some(Node::new_binary(NodeKind::Shl, node.unwrap(), rhs, nt));
                continue;
            }
            // ">>" add
            if token.equal(">>") {
                let rhs = self.next().add().unwrap();
                node = Some(Node::new_binary(NodeKind::Shr, node.unwrap(), rhs, nt));
                continue;
            }

            return node;
        }
    }

    /// 解析加减
    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Option<NodeLink> {
        // mul
        let mut node = self.mul();

        // ("+" mul | "-" mul)*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "+" mul
            if token.equal("+") {
                let rhs = self.next().mul().unwrap();
                node = add_with_type(node.unwrap(), rhs, nt);
                continue;
            }

            // "-" mul
            if token.equal("-") {
                let rhs = self.next().mul().unwrap();
                node = sub_with_type(node.unwrap(), rhs, nt);
                continue;
            }

            return node;
        }
    }

    /// 解析乘除
    /// mul = cast ("*" cast | "/" cast | "%" cast)*
    fn mul(&mut self) -> Option<NodeLink> {
        // unary
        let mut node = self.cast();

        // ("*" cast | "/" cast | "%" cast)*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // "*" unary
            if token.equal("*") {
                let rhs = self.next().cast().unwrap();
                node = Some(Node::new_binary(NodeKind::Mul, node.unwrap(), rhs, nt));
                continue;
            }

            // "/" cast
            if token.equal("/") {
                let rhs = self.next().cast().unwrap();
                node = Some(Node::new_binary(NodeKind::Div, node.unwrap(), rhs, nt));
                continue;
            }

            // "%" cast
            if token.equal("%") {
                let rhs = self.next().cast().unwrap();
                node = Some(Node::new_binary(NodeKind::Mod, node.unwrap(), rhs, nt));
                continue;
            }

            return node;
        }
    }

    /// cast = "(" typeName ")" cast | unary
    fn cast(&mut self) -> Option<NodeLink> {
        let (pos, token) = self.current();
        let next = self.get_token(pos + 1);
        if token.equal("(") && self.is_typename(next) {
            let typ = self.next().typename();
            self.skip(")");

            let (_, token) = self.current();
            // 复合字面量
            if token.equal("{") {
                self.cursor = pos;
                return self.unary();
            }

            // 解析嵌套的类型转换
            let cast = self.cast().unwrap();
            let node = Node::new_cast(cast, typ);
            return Some(node);
        }

        self.unary()
    }

    /// 解析一元运算
    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///         | ("++" | "--") unary
    ///         | postfix
    fn unary(&mut self) -> Option<NodeLink> {
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
                self.next().cast().unwrap(),
                nt,
            ));
        }

        // "&" cast
        if token.equal("&") {
            return Some(Node::new_unary(
                NodeKind::Addr,
                self.next().cast().unwrap(),
                nt,
            ));
        }

        // "*" cast
        if token.equal("*") {
            return Some(Node::new_unary(
                NodeKind::DeRef,
                self.next().cast().unwrap(),
                nt,
            ));
        }

        // "!" cast
        if token.equal("!") {
            return Some(Node::new_unary(
                NodeKind::Not,
                self.next().cast().unwrap(),
                nt,
            ));
        }

        // "~" cast
        if token.equal("~") {
            return Some(Node::new_unary(
                NodeKind::BitNot,
                self.next().cast().unwrap(),
                nt,
            ));
        }

        // 转换 ++i 为 i+=1
        // "++" unary
        if token.equal("++") {
            let unary = self.next().unary().unwrap();
            let node = add_with_type(unary, Node::new_num(1, nt.clone()), nt.clone()).unwrap();
            return self.to_assign(node);
        }

        // 转换 --i 为 i-=1
        // "--" unary
        if token.equal("--") {
            let unary = self.next().unary().unwrap();
            let node = sub_with_type(unary, Node::new_num(1, nt.clone()), nt.clone()).unwrap();
            return self.to_assign(node);
        }

        // primary
        self.postfix()
    }

    /// 获取枚举类型信息
    /// enumSpecifier = ident? "{" enum_list? "}"
    ///               | ident ("{" enum_list? "}")?
    /// enum_list     = ident ("=" const_expr)? ("," ident ("=" const_expr)?)* ","?
    fn enum_specifier(&mut self) -> TypeLink {
        let typ = Type::new_enum();

        let mut tag = None;
        let (_, tag_token) = self.current();
        if tag_token.is_ident() {
            tag = Some(tag_token.clone());
            self.next();
        }

        let (_, token) = self.current();
        if tag.is_some() && !token.equal("{") {
            let typ = self.find_tag(&tag.as_ref().unwrap().get_name().to_string());
            if typ.is_none() {
                error_token!(&tag.unwrap(), "unknown enum type");
                unreachable!()
            }
            let typ = typ.unwrap();
            if typ.borrow().kind != TypeKind::Enum {
                error_token!(&tag.unwrap(), "not an enum tag");
                unreachable!()
            }
            return typ;
        }

        self.skip("{");

        let mut i = 0;
        let mut val = 0;
        while !self.consume_end() {
            if i > 0 {
                self.skip(",");
            }
            i += 1;
            let (_, token) = self.current(); // 重新取,可能被跳过了,
            let name = token.get_name().to_string();
            self.next(); // 跳过name

            // 判断是否存在赋值
            let (_, token) = self.current(); // 重新取
            if token.equal("=") {
                val = self.next().const_expr();
            }

            let vs = self.push_scope(name);
            {
                let mut vsm = vs.as_ref().borrow_mut();
                vsm.set_enum(typ.clone(), val)
            }
            val += 1;
        }

        if tag.is_some() {
            let tag_name = tag.unwrap().get_name().to_string();
            self.push_tag_scope(tag_name, typ.clone())
        }

        typ
    }

    /// struct_members = (declspec declarator (","  declarator)* ";")*
    fn struct_members(&mut self, typ: &mut Type) {
        let mut members = vec![];

        let mut idx = 0;
        loop {
            let (_, token) = self.current();
            if token.equal("}") {
                break;
            }

            let mut attr = Some(VarAttr::new());
            let base_type = self.declspec(&mut attr);
            let mut is_first = true;

            while !self.consume(";") {
                if !is_first {
                    self.skip(",");
                }
                is_first = false;

                let typ = self.declarator(base_type.clone());
                let name = typ.borrow().get_name().to_string();
                let mut member = Member::new(name.to_string(), Some(typ), idx);
                idx += 1;
                let va = attr.as_ref().unwrap();
                if va.align != 0 {
                    member.align = va.align;
                } else {
                    member.align = member.typ.as_ref().unwrap().borrow().align;
                }
                members.push(member);
            }
        }

        // 解析灵活数组成员，数组大小设为0
        if members.len() > 0 {
            let last = members.len() - 1;
            let mut t = members[last].typ.as_ref().unwrap().borrow_mut();
            if t.kind == TypeKind::Array && t.len < 0 {
                let new_type = Type::array_of0(t.base.as_ref().unwrap().clone(), 0);
                *t = new_type;
                // 设置类型为灵活的
                typ.is_flexible = true;
            }
        }

        self.next();
        typ.members = members;
    }

    /// struct_declare = struct_union_declare
    fn struct_declare(&mut self) -> TypeLink {
        let typ = self.struct_union_declare();
        let mut tm = typ.borrow_mut();
        tm.kind = TypeKind::Struct;

        // 不完整结构体
        if tm.size < 0 {
            return typ.clone();
        }

        let mut offset: isize = 0;

        for i in 0..tm.members.len() {
            let member = &mut tm.members[i];
            let align = member.align;
            offset = align_to(offset, align);
            member.set_offset(offset);
            offset += member.typ.as_ref().unwrap().borrow().size;

            if tm.align < align {
                tm.align = align;
            }
        }
        tm.size = align_to(offset, tm.align);

        typ.clone()
    }

    /// union_declare = struct_union_declare
    fn union_declare(&mut self) -> TypeLink {
        let typ = self.struct_union_declare();
        let mut tm = typ.borrow_mut();
        tm.kind = TypeKind::Union;

        // 联合体需要设置为最大的对齐量与大小，变量偏移量都默认为0
        for i in 0..tm.members.len() {
            let member = &mut tm.members[i];
            let align = member.align;
            let size = member.typ.as_ref().unwrap().borrow().size;
            if tm.align < align {
                tm.align = align;
            }
            if tm.size < size {
                tm.size = size;
            }
        }
        tm.size = align_to(tm.size, tm.align);

        typ.clone()
    }

    /// struct_union_declare = ident? ("{" struct_members)?
    fn struct_union_declare(&mut self) -> TypeLink {
        let mut tag = None;
        let mut name = None;
        let (_, tag_token) = self.current();
        if tag_token.is_ident() {
            tag = Some(tag_token.clone());
            name = Some(tag_token.get_name().to_string());
            self.next();
        }

        // 构造不完整结构体
        let (_, token) = self.current();
        if tag.is_some() && !token.equal("{") {
            let typ = self.find_tag(name.as_ref().unwrap());
            if typ.is_some() {
                return typ.unwrap();
            }

            let mut typ = Type::new_union_struct();
            typ.size = -1;
            typ.name = tag.unwrap().clone();
            let typ = Rc::new(RefCell::new(typ));
            self.push_tag_scope(name.unwrap(), typ.clone());
            return typ;
        }

        // ("{" structMembers)?
        self.skip("{");

        let mut typ = Type::new_union_struct();
        self.struct_members(&mut typ);
        typ.align = 1;

        // 如果是重复定义，就覆盖之前的定义。否则有名称就注册结构体类型
        if tag.is_some() {
            // 当前域里找
            let s = &mut self.scopes[0];
            let t = s.replace_tag(name.as_ref().unwrap(), typ.clone());
            if t.is_some() {
                return t.unwrap();
            }

            let typ = Rc::new(RefCell::new(typ.clone()));
            self.push_tag_scope(name.unwrap(), typ);
        }

        let typ = Rc::new(RefCell::new(typ.clone()));
        typ
    }

    // 获取结构体成员
    fn get_struct_member(&mut self, typ: &TypeLink, token: &Token) -> Option<Box<Member>> {
        for member in typ.borrow().members.iter() {
            if token.equal(member.name.as_str()) {
                return Some(member.clone());
            }
        }
        error_token!(token, "no such member");
        None
    }

    /// 构建结构体成员的节点
    fn struct_ref(&mut self, mut lhs: NodeLink) -> Option<NodeLink> {
        add_type(&mut lhs);

        let lhs_t = lhs.typ.as_ref().unwrap().clone();
        if lhs_t.borrow().kind != TypeKind::Struct && lhs_t.borrow().kind != TypeKind::Union {
            error_token!(&lhs.token, "not a struct nor a union");
        }

        let lhs_t = lhs.typ.as_ref().unwrap().clone();
        let (pos, _) = self.current();
        let nt = self.tokens[pos].clone();
        let mut node = Node::new_unary(NodeKind::Member, lhs, nt.clone());
        node.member = Some(self.get_struct_member(&lhs_t, &nt).unwrap());

        Some(node)
    }

    /// 转换 A++ 为 `(typeof A)((A += 1) - 1)`
    /// increase decrease
    fn inc_dec(&mut self, mut node: NodeLink, addend: i64) -> Option<NodeLink> {
        add_type(&mut node);
        let token = &node.token.clone();
        let typ = &node.typ.as_ref().unwrap().clone();
        let num = Node::new_num(addend, token.clone());
        let num_neg = Node::new_num(-addend, token.clone());
        let add = add_with_type(node, num, token.clone()).unwrap();
        let lhs = self.to_assign(add).unwrap();
        let add = add_with_type(lhs, num_neg, token.clone()).unwrap();
        let cast = Node::new_cast(add, typ.clone());
        Some(cast)
    }

    /// postfix "(" typename ")" "{" initializer_list "}"
    ///         = ident "(" func_args ")" postfix_tail*
    ///         | primary postfix_tail*
    ///
    /// postfix_tail = "[" expr "]"
    ///             | "(" func_args ")"
    ///             | "." ident
    ///             | "->" ident
    ///             | "++"
    ///             | "--"
    fn postfix(&mut self) -> Option<NodeLink> {
        //  "(" typename ")" "{" initializer_list "}"
        let (pos, token) = self.current();
        let start = token.clone();
        let next = self.get_token(pos + 1);
        if token.equal("(") && self.is_typename(next) {
            // 复合字面量
            let typ = self.next().typename();
            self.skip(")");

            if self.scopes.len() == 1 {
                let var = self.new_anon_gvar(typ);
                self.gvar_initializer(var.clone());
                let (end_pos, _) = self.current();
                self.cursor = pos;
                let node = Some(Node::new_var(var, start));
                self.cursor = end_pos;
                return node;
            }

            let nvar = self.new_lvar("".to_string(), typ).unwrap();
            let lhs = self.lvar_initializer(nvar.clone()).unwrap();
            let (_, token2) = self.current();
            let rhs = Node::new_var(nvar, token2.clone());
            let (end_pos, _) = self.current();
            self.cursor = pos;
            let node = Some(Node::new_binary(NodeKind::Comma, lhs, rhs, start));
            self.cursor = end_pos;
            return node;
        }

        // primary
        let mut node = self.primary().unwrap();

        // ("[" expr "]")*
        loop {
            let (pos, token) = self.current();
            let nt = self.tokens[pos].clone();
            // ident "(" funcArgs ")"
            // 匹配到函数调用
            if token.equal("(") {
                node = self.next().func_call(node).unwrap();
                continue;
            }

            if token.equal("[") {
                // x[y] 等价于 *(x+y)
                let idx = self.next().expr().unwrap();
                self.skip("]");
                let unary = add_with_type(node, idx, nt.clone()).unwrap();
                node = Node::new_unary(NodeKind::DeRef, unary, nt);
                continue;
            }

            // "." ident
            if token.equal(".") {
                node = self.next().struct_ref(node).unwrap();
                self.next();
                continue;
            }

            // "->" ident
            if token.equal("->") {
                node = Node::new_unary(NodeKind::DeRef, node, nt);
                node = self.next().struct_ref(node).unwrap();
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
    ///         | "_Alignof" "(" typename ")"
    ///         | "_Alignof" unary
    ///         | ident funcArgs?
    ///         | str
    ///         | num
    fn primary(&mut self) -> Option<NodeLink> {
        // "(" "{" stmt+ "}" ")"
        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        let next = self.get_token(pos + 1);
        if token.equal("(") && next.equal("{") {
            // This is a GNU statement expresssion.
            self.next().next();
            let body = self.compound_stmt().unwrap().body;
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
        let next_next = self.get_token(pos + 2);
        if token.equal(KW_SIZEOF) && next.equal("(") && self.is_typename(next_next) {
            self.next().next();
            let typ = self.typename();
            self.skip(")");
            // self.cursor = pos;
            return Some(Node::new_unsigned_long(typ.borrow().size as i64, nt));
        }

        // "sizeof" unary
        if token.equal(KW_SIZEOF) {
            let mut node = self.next().unary();
            add_type(node.as_mut().unwrap());
            let size = node.unwrap().get_type().as_ref().unwrap().borrow().size as i64;
            return Some(Node::new_unsigned_long(size, nt));
        }

        // "_Alignof" "(" typename ")"
        // 读取类型的对齐值
        if token.equal(KW_ALIGNOF) && next.equal("(") && self.is_typename(next_next) {
            let typ = self.next().next().typename();
            self.skip(")");
            return Some(Node::new_unsigned_long(typ.borrow().align as i64, nt));
        }

        // "_Alignof" unary
        if token.equal(KW_ALIGNOF) {
            let mut node = self.next().unary();
            add_type(node.as_mut().unwrap());
            let (pos, _) = self.current();
            let nt = self.tokens[pos].clone();
            let align = node.unwrap().get_type().as_ref().unwrap().borrow().align as i64;
            return Some(Node::new_unsigned_long(align, nt));
        }

        // ident
        if token.is_ident() {
            let name = token.get_name();
            // 查找变量
            let vso = self.find_var(&name);
            self.next();
            let node;
            if vso.is_some() {
                let vs = vso.unwrap().clone();
                let vsb = vs.borrow();
                let var = &vsb.var;
                let enum_type = &vsb.enum_type;
                let enum_val = &vsb.enum_val;
                if var.is_some() {
                    node = Node::new_var(var.as_ref().unwrap().clone(), nt);
                    return Some(node);
                }
                if enum_type.is_some() {
                    node = Node::new_num(*enum_val, nt);
                    return Some(node);
                }
            }
            let (_, token) = self.current();
            if token.equal(")") {
                error_token!(token, "implicit declaration of a function")
            }
            error_token!(token, "undefined variable");
            return None;
        } else if token.is_string() {
            let (val, typ) = token.get_string();
            let var = self.new_string_literal(vec_u8_into_i8(val.to_vec()), typ.clone());
            let node = Node::new_var(var, nt);
            self.next();
            return Some(node);
        } else if token.is_num() {
            let (val, fval, typ) = token.get_num();

            let mut node;
            if typ.borrow().is_float() {
                // 浮点数节点
                node = Node::new(NodeKind::Num, nt);
                node.fval = fval;
            } else {
                // 整型节点
                node = Node::new_num(val, nt);
            }
            // 设置类型为终结符的类型
            node.typ = Some(typ.clone());
            self.next();
            return Some(node);
        }

        let (_, token) = self.current();
        error_token!(token, "expected an expression");

        None
    }

    /// typename = declspec abstract_declarator
    fn typename(&mut self) -> TypeLink {
        let typ = self.declspec(&mut None);

        return self.abstract_declarator(typ);
    }

    /// abstract_declarator = pointers ("(" abstract_declarator ")")? type_suffix
    fn abstract_declarator(&mut self, mut base_type: TypeLink) -> TypeLink {
        // pointers
        base_type = self.pointers(base_type);

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

    // func_call = (assign ("," assign)*)? ")"
    fn func_call(&mut self, mut func: NodeLink) -> Option<NodeLink> {
        add_type(&mut func);

        let (pos, token) = self.current();
        let nt = self.tokens[pos].clone();
        let ft = func.typ.as_ref().unwrap().clone();
        let ftyp = ft.borrow();
        if !ftyp.is_func() {
            error_token!(token, "not a function")
        }

        let typ = if ftyp.kind == TypeKind::Func {
            ft.clone()
        } else {
            ftyp.get_base_type().unwrap()
        };

        let t = typ.clone();
        let binding = t.borrow();
        // 函数形参的类型 func_call时是正向的所以需要reverse一下
        let params: Vec<_> = binding.params.iter().rev().collect();

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
                let param = params[i];
                if param.borrow().kind == TypeKind::Struct || param.borrow().kind == TypeKind::Union
                {
                    error_token!(&arg.token, "passing struct or union is not supported yet");
                    unreachable!()
                }

                arg = Node::new_cast(arg, param.clone());
                i += 1;
            } else if arg.typ.as_ref().unwrap().borrow().kind == TypeKind::Float {
                // 若无形参类型，浮点数会被提升为double
                arg = Node::new_cast(arg, Type::new_double());
            }

            add_type(&mut arg);
            nodes.push(arg);
        }

        self.skip(")");

        let mut node = Node::new_unary(NodeKind::FuncCall, func, nt);
        node.func_type = Some(t.clone());
        let tb = t.borrow();
        node.typ = Some(tb.return_type.as_ref().unwrap().clone());
        node.args = nodes;
        Some(node)
    }

    /// 进入新的域
    fn enter_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.insert(0, scope);
    }

    /// 结束当前域
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
    fn find_tag(&self, name: &String) -> Option<TypeLink> {
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

    /// 解析类型别名
    fn parse_typedef(&mut self, base_type: TypeLink) {
        let mut first = true;

        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;
            let typ = self.declarator(base_type.clone());
            if typ.borrow().name.is_null() {
                error_token!(&typ.borrow().name_pos, "variable name omitted");
            }
            let name = typ.borrow().get_name().to_string();

            let vs = self.push_scope(name);
            let mut vsm = vs.as_ref().borrow_mut();
            vsm.set_typedef(typ);
        }
    }

    /// 判断是否终结符匹配到了结尾
    fn is_end(&mut self) -> bool {
        let (pos, token) = self.current();
        let next = self.get_token(pos + 1);
        // "}" | ",}"
        return token.equal("}") || (token.equal(",") && next.equal("}"));
    }

    /// 消耗掉结尾的终结符
    /// "}" | ",}"
    fn consume_end(&mut self) -> bool {
        let (pos, token) = self.current();
        // "}"
        if token.equal("}") {
            self.next();
            return true;
        }

        // ",}"
        let next = self.get_token(pos + 1);
        if token.equal(",") && next.equal("}") {
            self.next().next();
            return true;
        }
        return false;
    }

    /// 判断是否是方法,要回档哦
    fn is_function(&mut self) -> bool {
        let (start, token) = self.current();
        if token.equal(";") {
            return false;
        }
        let typ = self.declarator(Type::new_int());
        // 游标回档
        self.cursor = start;
        return typ.borrow().kind == TypeKind::Func;
    }

    fn new_anon_gvar(&mut self, base_type: TypeLink) -> ObjLink {
        let name = self.new_unique_name();
        let obj = Obj::new_gvar(name.to_string(), base_type);
        self.new_gvar(name, obj).unwrap()
    }

    /// 新增字符串字面量
    fn new_string_literal(&mut self, str_data: Vec<i8>, base_type: TypeLink) -> ObjLink {
        let gvar = self.new_anon_gvar(base_type);
        gvar.borrow_mut().set_init_data(str_data);
        gvar
    }

    /// 获取新的唯一名称
    pub fn new_unique_name(&mut self) -> String {
        let name = format!(".L..{}", self.unique_idx);
        self.unique_idx += 1;
        name
    }
}
