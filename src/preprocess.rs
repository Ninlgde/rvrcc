//! 预处理

use crate::cmacro::{HideSet, Macro, MacroArg, MacroParam, BUILTIN_MACROS};
use crate::parse::Parser;
use crate::token::{File, Token, TokenVecOps};
use crate::tokenize::{convert_pp_tokens, tokenize};
use crate::{
    dirname, error_token, file_exists, join_adjacent_string_literals, tokenize_file, warn_token,
};

/// 预处理器入口函数
pub fn preprocess(tokens: &mut Vec<Token>, include_path: Vec<String>) -> Vec<Token> {
    // 处理宏和指示
    let mut processor = Preprocessor::new(tokens, include_path);
    let mut tokens = processor.process();
    // 将所有关键字的终结符，都标记为KEYWORD, ppnum->num
    convert_pp_tokens(&mut tokens);

    let tokens = join_adjacent_string_literals(tokens);
    tokens
}

/// 预处理器
struct Preprocessor<'a> {
    /// 输入的tokens
    tokens: &'a mut Vec<Token>,
    /// 游标
    cursor: usize,
    /// 结果
    result: Vec<Token>,
    /// 全局的#if保存栈
    cond_incls: Vec<CondIncl>,
    /// 宏变量栈
    macros: Vec<Macro>,
    /// 引入路径区
    include_path: Vec<String>,
}

impl TokenVecOps for Preprocessor<'_> {
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

impl<'a> Preprocessor<'a> {
    /// 构造预处理器
    fn new(tokens: &'a mut Vec<Token>, include_path: Vec<String>) -> Self {
        Self {
            tokens,
            cursor: 0,
            result: vec![],
            cond_incls: vec![],
            macros: unsafe { BUILTIN_MACROS.to_vec() },
            include_path,
        }
    }

    /// 从其他预处理构造新的处理器
    fn from(processor: &Self, tokens: &'a mut Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            result: vec![],
            cond_incls: processor.cond_incls.to_vec(),
            macros: processor.macros.to_vec(),
            include_path: processor.include_path.to_vec(),
        }
    }

    /// 处理
    fn process(&mut self) -> Vec<Token> {
        // 处理宏和指示
        self.process0();

        // 此时#if应该都被清空了，否则报错
        if self.cond_incls.len() != 0 {
            let last = self.cond_incls.last().unwrap();
            error_token!(&last.token, "unterminated conditional directive");
        }

        self.result.to_vec()
    }

    /// 处理,但不检查#if堆栈
    fn process_without_check(&mut self) -> Vec<Token> {
        // 处理宏和指示
        self.process0();
        self.result.to_vec()
    }

    /// 获取当前游标的token
    fn current_token_mut(&mut self) -> &mut Token {
        &mut self.tokens[self.cursor]
    }

    /// 处理
    fn process0(&mut self) {
        while !self.finished() {
            // 如果是个宏变量，那么就展开
            if self.expand_macro() {
                continue;
            }

            let token = self.current_token();
            if !token.is_hash() {
                // 如果不是#号开头则添加
                self.add_token(token.clone());
                self.next();
                continue;
            }
            let start = self.current_token().clone();
            let token = self.next().current_token();

            if token.equal("include") {
                // 讲这行整体copy出来
                let tokens = self.next().copy_line();
                // 是否有双引号
                let mut is_dqueto = false;
                let filename = read_include_filename(self, tokens, &mut is_dqueto);
                // 不以"/"开头的视为相对路径
                if !filename.starts_with("/") && is_dqueto {
                    // 以当前文件所在目录为起点
                    // 路径为：终结符文件名所在的文件夹路径/当前终结符名
                    let dir = dirname(start.get_file_name());
                    let path = format!("{}/{}", dir, filename);
                    if file_exists(&path) {
                        self.include_file(path.to_string(), &start);
                        continue;
                    }
                }

                // 直接引入文件
                let path = self.search_include_paths(filename.to_string());
                self.include_file(path, &start);
                continue;
            }

            // 匹配#define
            if token.equal("define") {
                // 读取宏定义
                self.next().read_macro_definition();
                continue;
            }

            // 匹配#undef
            if token.equal("undef") {
                let token = self.next().current_token();
                // 如果匹配到的不是标识符就报错
                if !token.is_ident() {
                    error_token!(token, "macro name must be an identifier");
                }
                // 获取name
                let name = token.get_name();
                // 跳到行首
                self.next().skip_line();
                // 压入一个被标记删除的宏变量
                self.add_delete_macro(name.as_str());
                continue;
            }

            // 匹配#if
            if token.equal("if") {
                // 计算常量表达式
                let val = self.eval_const_expr();
                // 将Tok压入#if栈中
                self.push_cond_incl(start, val != 0);
                // 处理#if后值为假的情况，全部跳过
                if val == 0 {
                    self.skip_cond_incl();
                }
                continue;
            }

            // 匹配#ifdef
            if token.equal("ifdef") {
                let t = self.current_token().clone();
                // 查找宏变量
                self.next();
                let next = self.current_token();
                let defined = self.find_macro(next);
                // 压入#if栈
                self.push_cond_incl(t, defined.is_some());
                // 跳到行首
                self.next().skip_line();
                // 如果没被定义，那么应该跳过这个部分
                if defined.is_none() {
                    self.skip_cond_incl();
                }
                continue;
            }

            // 匹配#ifndef
            if token.equal("ifndef") {
                let t = self.current_token().clone();
                // 查找宏变量
                self.next();
                let next = self.current_token();
                let defined = self.find_macro(next);
                // 压入#if栈，此时不存在时则设为真
                self.push_cond_incl(t, defined.is_none());
                // 跳到行首
                self.next().skip_line();
                // 如果被定义了，那么应该跳过这个部分
                if defined.is_some() {
                    self.skip_cond_incl();
                }
                continue;
            }

            // 匹配#elif
            if token.equal("elif") {
                if self.cond_incls.len() == 0
                    || self.cond_incls.last().unwrap().ctx == CondInclKind::Else
                {
                    error_token!(&start, "stray #elif");
                }
                let last = self.cond_incls.last_mut().unwrap();
                last.ctx = CondInclKind::Elif;

                if !last.included && self.eval_const_expr() != 0 {
                    // 处理之前的值都为假且当前#elif为真的情况
                    self.cond_incls.last_mut().unwrap().included = true;
                } else {
                    // 否则其他的情况，全部跳过
                    self.skip_cond_incl();
                }
                continue;
            }

            // 匹配#else
            if token.equal("else") {
                if self.cond_incls.len() == 0
                    || self.cond_incls.last().unwrap().ctx == CondInclKind::Else
                {
                    error_token!(&start, "stray #else");
                }
                self.cond_incls.last_mut().unwrap().ctx = CondInclKind::Else;
                // 走到行首
                self.next().skip_line();

                // 处理之前有值为真的情况，则#else全部跳过
                if self.cond_incls.last().unwrap().included {
                    self.skip_cond_incl();
                }
                continue;
            }

            // 匹配#endif
            if token.equal("endif") {
                // 弹栈，失败报错
                if self.cond_incls.len() == 0 {
                    error_token!(&start, "stray #endif");
                }
                self.cond_incls.pop();
                // 走到行首
                self.next().skip_line();
                continue;
            }

            // 匹配#error
            if token.equal("error") {
                error_token!(token, "error");
            }

            if token.at_bol() {
                continue;
            }

            error_token!(token, "invalid preprocessor directive");
        }
    }

    /// 引入文件
    fn include_file(&mut self, path: String, file_token: &Token) {
        // 词法分析文件
        let include_tokens = tokenize_file(path.to_string());
        if include_tokens.len() <= 1 {
            error_token!(file_token, "{}: cannot open file", &path);
        }
        self.append_tokens(include_tokens);
    }

    // 搜索引入路径区
    fn search_include_paths(&self, filename: String) -> String {
        if filename.starts_with("/") {
            return filename;
        }

        // 从引入路径区查找文件
        for incl in self.include_path.iter() {
            let path = format!("{}/{}", incl, filename);
            if file_exists(&path) {
                return path;
            }
        }
        // 啥也没找到,直接返回吧
        filename
    }

    /// 检查结束
    fn finished(&self) -> bool {
        // self.current_token().at_eof()
        self.cursor == self.tokens.len()
    }

    /// 向结果添加token
    fn add_token(&mut self, token: Token) {
        self.result.push(token);
    }

    /// 向结果添加tokens
    fn append_tokens(&mut self, tokens: Vec<Token>) {
        for token in tokens.iter().rev() {
            if !token.at_eof() {
                self.tokens.insert(self.cursor, Token::form(token));
            }
        }
    }

    /// 一些预处理器允许#include等指示，在换行前有多余的终结符
    /// 此函数跳过这些终结符
    fn skip_line(&mut self) {
        let token = self.current_token();
        if token.at_bol() {
            return;
        }

        warn_token!(token, "extra token");

        while !self.current_token().at_bol() {
            self.next();
        }
    }

    /// 拷贝当前Tok到换行符间的所有终结符，并以EOF终结符结尾
    /// 此函数为#if分析参数
    fn copy_line(&mut self) -> Vec<Token> {
        let mut result = vec![];

        while !self.current_token().at_bol() {
            result.push(Token::form(self.current_token()));
            self.next();
        }

        result.push(Token::new_eof(0, 0));
        result
    }

    /// 读取并计算常量表达式
    fn eval_const_expr(&mut self) -> i64 {
        let (_, start) = self.current();
        // clone走,否则触发借用error
        let start = start.clone();
        // 解析#if后的常量表达式
        let mut tokens = self.next().read_const_expr();
        // 对于宏变量进行解析
        let mut processor = Preprocessor::from(self, &mut tokens);
        let mut expr = processor.process_without_check();
        if expr.len() <= 1 {
            error_token!(&start, "no expression");
        }

        for ex in expr.iter_mut() {
            if ex.is_ident() {
                let nt = new_num_token(0, ex);
                *ex = nt;
            }
        }

        // 转换预处理数值到正常数值
        convert_pp_tokens(&mut expr);

        // 计算常量表达式的值
        let mut parser = Parser::new(&expr);
        let val = parser.const_expr();
        let (_, pt) = parser.current();
        if !pt.at_eof() {
            error_token!(pt, "extra token");
        }
        val
    }

    /// 读取常量表达式
    fn read_const_expr(&mut self) -> Vec<Token> {
        let tokens = self.copy_line();
        let mut result = vec![];

        let mut i = 0;
        loop {
            let token = &tokens[i];
            if token.at_eof() {
                break;
            }
            // "defined(foo)" 或 "defined foo"如果存在foo为1否则为0
            if token.equal("defined") {
                let start = token;
                // 消耗掉(
                i += 1;
                let token = &tokens[i];
                let has_param = token.equal("(");
                if has_param {
                    i += 1;
                }
                let token = &tokens[i];
                if !token.is_ident() {
                    error_token!(start, "macro name must be an identifier");
                }
                let macro_ = self.find_macro(token);
                i += 1;
                if has_param {
                    if tokens[i].equal(")") {
                        i += 1;
                    } else {
                        error_token!(start, "expect ')'");
                    }
                }
                // 构造一个相应的数字终结符
                let val = if macro_.is_some() { 1 } else { 0 };
                let nt = new_num_token(val, start);
                result.push(nt);
                continue;
            }
            // 将剩余的终结符存入链表
            result.push(token.clone());
            i += 1;
        }

        result.push(Token::new_eof(0, 0));

        result
    }

    /// 压入#if栈中
    fn push_cond_incl(&mut self, token: Token, included: bool) {
        let ci = CondIncl {
            ctx: CondInclKind::Then,
            token,
            included,
        };
        self.cond_incls.push(ci);
    }

    /// #if为空时，一直跳过到#endif
    /// 其中嵌套的#if语句也一起跳过
    fn skip_cond_incl(&mut self) {
        while !self.finished() {
            let (pos, token) = self.current();
            let next = &self.tokens[pos + 1];
            // 跳过#if语句
            if token.is_hash() && (next.equal("if") || next.equal("ifdef") || next.equal("ifndef"))
            {
                self.next().next().skip_cond_incl2();
                continue;
            }
            // #endif
            if token.is_hash() && (next.equal("elif") || next.equal("else") || next.equal("endif"))
            {
                break;
            }
            self.next();
        }
    }

    /// 跳过#if和#endif
    fn skip_cond_incl2(&mut self) {
        while !self.finished() {
            let (pos, token) = self.current();
            let next = &self.tokens[pos + 1];
            // 跳过#if语句
            if token.is_hash() && (next.equal("if") || next.equal("ifdef") || next.equal("ifndef"))
            {
                self.next().next().skip_cond_incl2();
                continue;
            }
            // #endif
            if token.is_hash() && next.equal("endif") {
                self.next().next();
                return;
            }
            self.next();
        }
    }

    /// 通过token查找宏
    fn find_macro(&self, token: &Token) -> Option<Macro> {
        if !token.is_ident() {
            return None;
        }

        let name = token.get_name();
        // 栈需要倒序查找,越top越优先
        for macro_ in self.macros.iter().rev() {
            // 相等且未被标记删除,才返回
            if macro_.eq(name.as_str()) {
                if macro_.deleted() {
                    return None;
                }
                return Some(macro_.clone());
            }
        }
        None
    }

    /// 新增宏变量，压入宏变量栈中
    fn add_macro(&mut self, name: &str, is_obj_like: bool, body: Vec<Token>) -> Macro {
        let macro_ = Macro::new(name, body, false, is_obj_like);
        self.macros.push(macro_.clone());
        macro_
    }

    /// 压入一个被标记删除的宏变量
    fn add_delete_macro(&mut self, name: &str) {
        let macro_ = Macro::new(name, vec![], true, true);
        self.macros.push(macro_);
    }

    /// 如果是宏变量并展开成功，返回真
    fn expand_macro(&mut self) -> bool {
        let (pos, token) = self.current();
        let macro_token = self.current_token().clone();
        // 判断是否处于隐藏集之中
        if token.contain_hide_set() {
            return false;
        }
        let macro_ = self.find_macro(token);
        if macro_.is_none() {
            return false;
        }
        let macro_ = macro_.unwrap();
        // 如果宏设置了相应的处理函数，例如__LINE__
        if macro_.get_handler().is_some() {
            // 就使用相应的处理函数解析当前的宏
            let func = macro_.get_handler().unwrap();
            let nts = func(vec![token.clone()]);
            self.next().append_tokens(nts);
            return true;
        }

        let macro_name = macro_.get_name();
        // 为宏变量时
        if macro_.is_obj_like() {
            // 展开过一次的宏变量，就加入到隐藏集当中
            let token = self.current_token_mut();
            token.add_hide_set(HideSet::new(macro_name));

            // 处理此宏变量之后，传递隐藏集给之后的终结符
            let hs = token.get_hide_set();
            let body = macro_.get_body();
            let mut rb = add_hide_set(body, hs);
            for b in rb.iter_mut() {
                b.set_origin(macro_token.clone());
            }
            // 传递 是否为行首 和 前面是否有空格 的信息
            rb[0].set_bol_space(token.at_bol(), token.has_space());
            self.next().append_tokens(rb);
            return true;
        }

        // 如果宏函数后面没有参数列表，就处理为正常的标识符
        let next = &self.tokens[pos + 1];
        if !next.equal("(") {
            return false;
        }

        // 处理宏函数，并连接到Tok之后
        // 读取宏函数实参，这里是宏函数的隐藏集
        let args = self.read_macro_args(&macro_.get_params(), macro_.get_variadic());
        // 这里返回的是右括号，这里是宏参数的隐藏集
        let r_paren = self.current_token().clone();
        // 宏函数间可能具有不同的隐藏集，新的终结符就不知道应该使用哪个隐藏集。
        // 我们取宏终结符和右括号的交集，并将其用作新的隐藏集。
        let hs = HideSet::intersection(macro_token.get_hide_set(), r_paren.get_hide_set());

        // 将当前函数名加入隐藏集
        let hs = HideSet::union(hs, HideSet::new(macro_name.to_string()));
        // 替换宏函数内的形参为实参
        let body = subst(self, macro_.get_body(), args);
        // 为宏函数内部设置隐藏集
        let mut rb = add_hide_set(body, hs);
        for b in rb.iter_mut() {
            b.set_origin(macro_token.clone());
        }
        // 传递 是否为行首 和 前面是否有空格 的信息
        rb[0].set_bol_space(macro_token.at_bol(), macro_token.has_space());
        // 将设置好的宏函数内部连接到终结符链表中
        self.next().append_tokens(rb);
        true
    }

    /// 读取宏定义
    fn read_macro_definition(&mut self) {
        let token = self.current_token();
        // 如果匹配到的不是标识符就报错
        if !token.is_ident() {
            error_token!(token, "macro name must be an identifier");
        }
        let name = token.get_name();
        let token = self.next().current_token();

        // 判断是宏变量还是宏函数，括号前没有空格则为宏函数
        if !token.has_space() && token.equal("(") {
            // 构造形参
            let mut is_variadic = false;
            let params = self.next().read_macro_params(&mut is_variadic);
            let body = self.copy_line();
            // 增加宏函数
            let mut macro_ = self.add_macro(&name, false, body);
            macro_.set_params(params);
            macro_.set_variadic(is_variadic);
        } else {
            // 增加宏变量
            let body = self.copy_line();
            self.add_macro(&name, true, body);
        };
    }

    /// 读取宏形参
    fn read_macro_params(&mut self, is_variadic: &mut bool) -> Vec<MacroParam> {
        let mut params = vec![];

        while !self.current_token().equal(")") {
            if params.len() > 0 {
                self.skip(",");
            }
            let token = self.current_token();
            // 处理可变参数
            if token.equal("...") {
                *is_variadic = true;
                // "..."应为最后一个参数
                self.next().skip(")");
                return params;
            }
            // 如果不是标识符报错
            if !token.is_ident() {
                error_token!(token, "expected an identifier");
            }
            // 创建macro param
            let param = MacroParam {
                name: token.get_name(),
            };
            params.push(param);
            self.next();
        }
        self.next();
        params
    }

    /// 读取单个宏实参
    fn read_macro_arg_one(&mut self, read_rest: bool) -> MacroArg {
        let mut tokens = vec![];

        let mut level = 0;
        // 读取实参对应的终结符
        loop {
            let token = self.current_token();
            // 终止条件
            if level == 0 && token.equal(")") {
                break;
            }
            // 在这里ReadRest为真时，则可以读取多个终结符
            if level == 0 && !read_rest && token.equal(",") {
                break;
            }
            if token.at_eof() {
                error_token!(token, "premature end of input");
            }
            if token.equal("(") {
                level += 1;
            } else if token.equal(")") {
                level -= 1;
            }
            // 将标识符加入到链表中
            tokens.push(Token::form(token));
            self.next();
        }
        // 加入EOF终结
        tokens.push(Token::new_eof(0, 0));

        MacroArg {
            name: "".to_string(),
            tokens,
        }
    }

    /// 读取宏实参
    fn read_macro_args(&mut self, params: &Vec<MacroParam>, is_variadic: bool) -> Vec<MacroArg> {
        let start = self.current_token().clone();
        self.next().next();

        let mut args = vec![];
        // 遍历形参，然后对应着加入到实参链表中
        for param in params.iter() {
            if args.len() > 0 {
                self.skip(",");
            }
            // 读取单个实参
            let mut arg = self.read_macro_arg_one(false);
            // 设置为对应的形参名称
            arg.name = param.name.to_string();
            args.push(arg);
        }

        // 剩余未匹配的实参，如果为可变参数
        if is_variadic {
            let token = self.current_token();
            let mut arg;
            // 剩余实参为空
            if token.equal(")") {
                arg = MacroArg {
                    name: "".to_string(),
                    tokens: vec![Token::new_eof(0, 0)],
                };
            } else {
                // 处理对应可变参数的实参
                // 跳过","
                if args.len() > 0 {
                    self.skip(",");
                }
                arg = self.read_macro_arg_one(true);
            }
            arg.name = "__VA_ARGS__".to_string();
            args.push(arg);
        } else if args.len() < params.len() {
            error_token!(&start, "too many arguments");
        }
        self.require(")");
        args
    }
}

#[derive(Clone, PartialEq, Eq)]
enum CondInclKind {
    Then,
    Elif,
    Else,
}

/// #if可以嵌套，所以使用栈来保存嵌套的#if
#[derive(Clone)]
struct CondIncl {
    /// 类型
    ctx: CondInclKind,
    /// 对应的终结符
    token: Token,
    /// 是否被包含
    included: bool,
}

/// 向body中的每个token添加hideSet
fn add_hide_set(body: Vec<Token>, hs: *mut HideSet) -> Vec<Token> {
    let mut rb = vec![];
    for token in body.iter() {
        let mut nt = Token::form(token);
        nt.add_hide_set(hs);
        rb.push(nt);
    }
    rb
}

/// 遍历查找实参
fn find_arg<'a>(args: &'a Vec<MacroArg>, token: &'a Token) -> Option<&'a MacroArg> {
    for arg in args.iter() {
        if token.equal(arg.name.as_str()) {
            return Some(arg);
        }
    }
    None
}

/// 将宏函数形参替换为指定的实参
fn subst(processor: &Preprocessor, body: Vec<Token>, args: Vec<MacroArg>) -> Vec<Token> {
    let mut tokens = vec![];

    // 遍历将形参替换为实参的终结符链表
    let mut i = 0;
    while i < body.len() {
        let token = &body[i];
        // #宏实参 会被替换为相应的字符串
        if token.equal("#") {
            let hash = &body[i + 1];
            let arg = find_arg(&args, hash);
            if arg.is_none() {
                error_token!(token, "'#' is not followed by a macro parameter");
            }
            tokens.push(stringize(token, &arg.as_ref().unwrap().tokens));
            i += 2;
            continue;
        }

        // ##及右边，用于连接终结符
        if token.equal("##") {
            if tokens.len() == 0 {
                error_token!(token, "'##' cannot appear at start of macro expansion");
            }
            if i == body.len() - 1 {
                error_token!(token, "'##' cannot appear at end of macro expansion");
            }
            // 查找下一个终结符
            // 如果是（##右边）宏实参
            let hash = &body[i + 1];
            let arg = find_arg(&args, hash);
            if arg.is_some() {
                let arg = arg.unwrap();
                // 如果只有1,说明只有eof
                if arg.tokens.len() > 1 {
                    // 拼接当前终结符和（##右边）实参
                    let cur = tokens.pop().unwrap();
                    let mut ncur = paste(&cur, arg.tokens.first().unwrap());
                    // 传递 是否为行首 和 前面是否有空格 的信息
                    ncur.set_bol_space(cur.at_bol(), cur.has_space());
                    tokens.push(ncur);
                    // 将（##右边）实参未参与拼接的剩余部分加入到链表当中
                    for idx in 1..arg.tokens.len() {
                        let at = &arg.tokens[idx];
                        if !at.at_eof() {
                            tokens.push(Token::form(at));
                        }
                    }
                }
                i += 2;
                continue;
            }
            // 如果不是（##右边）宏实参
            // 直接拼接
            let next = &body[i + 1];
            let cur = tokens.pop().unwrap();
            let mut ncur = paste(&cur, next);
            // 传递 是否为行首 和 前面是否有空格 的信息
            ncur.set_bol_space(cur.at_bol(), cur.has_space());
            tokens.push(ncur);
            i += 2;
            continue;
        }

        // 查找实参
        let arg = find_arg(&args, token);

        // 左边及##，用于连接终结符
        if arg.is_some() && (&body[i + 1]).equal("##") {
            let arg = arg.unwrap();
            // 读取##右边的终结符
            let rhs = &body[i + 2];
            // 实参（##左边）为空的情况(只有1个eof)
            if arg.tokens.len() == 1 {
                // 查找（##右边）实参
                let arg2 = find_arg(&args, rhs);
                if arg2.is_some() {
                    // 如果是实参，那么逐个遍历实参对应的终结符
                    for a2t in arg2.unwrap().tokens.iter() {
                        if !a2t.at_eof() {
                            tokens.push(Token::form(a2t));
                        }
                    }
                } else {
                    // 如果不是实参，那么直接复制进链表
                    tokens.push(Token::form(rhs));
                }
                // 指向（##右边）实参的下一个
                i += 3;
                continue;
            }
            // 实参（##左边）不为空的情况
            for at in arg.tokens.iter() {
                if !at.at_eof() {
                    tokens.push(Token::form(at));
                }
            }
            i += 1;
            continue;
        }

        // 处理宏终结符，宏实参在被替换之前已经被展开了
        if arg.is_some() {
            let arg = arg.unwrap();
            let mut arg_tokens = arg.tokens.to_vec();
            let mut processor = Preprocessor::from(processor, &mut arg_tokens);
            let mut pts = processor.process_without_check();
            // 传递 是否为行首 和 前面是否有空格 的信息
            pts[0].set_bol_space(token.at_bol(), token.has_space());
            for pt in pts.iter() {
                if !pt.at_eof() {
                    tokens.push(Token::form(pt));
                }
            }
            i += 1;
            continue;
        }
        // 处理非宏的终结符
        tokens.push(Token::form(token));
        i += 1;
    }

    tokens
}

/// 将终结符链表中的所有终结符都连接起来，然后返回一个新的字符串
pub fn join_tokens(tokens: &Vec<Token>) -> String {
    let mut buf = String::new();
    for token in tokens.iter() {
        if buf.len() != 0 && token.has_space() {
            buf.push_str(" ");
        }
        buf.push_str(token.get_name().as_str());
    }

    buf
}

/// 将所有实参中的终结符连接起来，然后返回一个字符串的终结符
pub fn stringize(hash: &Token, tokens: &Vec<Token>) -> Token {
    // 创建一个字符串的终结符
    let string = join_tokens(tokens);
    // 我们需要一个位置用来报错，所以使用了宏的名字
    new_str_token(string, hash)
}

/// 构建一个新的字符串的终结符
pub fn new_str_token(s: String, tmpl: &Token) -> Token {
    // 将字符串加上双引号
    let buf = format!("{:?}", s);
    // 将字符串和相应的宏名称传入词法分析，去进行解析
    let file = File::new_link(tmpl.get_file_name(), tmpl.get_file_no(), buf);
    tokenize(file)[0].clone()
}

/// 构造数字终结符
pub fn new_num_token(val: i64, tmpl: &Token) -> Token {
    let buf = format!("{}", val);
    // 将字符串和相应的宏名称传入词法分析，去进行解析
    let file = File::new_link(tmpl.get_file_name(), tmpl.get_file_no(), buf);
    tokenize(file)[0].clone()
}

/// 拼接两个终结符构建一个新的终结符
pub fn paste(lhs: &Token, rhs: &Token) -> Token {
    // 合并两个终结符
    let buf = format!("{}{}", lhs.get_name(), rhs.get_name());
    // 词法解析生成的字符串，转换为相应的终结符
    let file = File::new_link(lhs.get_file_name(), lhs.get_file_no(), buf.to_string());
    let tokens = tokenize(file);
    if tokens.len() > 2 {
        error_token!(lhs, "pasting forms '{}', an invalid token", buf);
    }
    tokens[0].clone()
}

/// 读取#include参数
fn read_include_filename(
    processor: &Preprocessor,
    mut tokens: Vec<Token>,
    is_dquote: &mut bool,
) -> String {
    let token = &tokens[0];
    // 匹配样式1: #include "foo.h"
    if token.is_string() {
        // #include 的双引号文件名是一种特殊的终结符
        // 不能转义其中的任何转义字符。
        // 例如，“C:\foo”中的“\f”不是换页符，而是\和f。
        // 所以此处不使用token->str。
        *is_dquote = true;
        return token.get_string_literal();
    }

    // 匹配样式2: #include <foo.h>
    if token.equal("<") {
        // 从"<"和">"间构建文件名.
        let mut ots = vec![];
        // 查找">",并把<>中间的token都加入到tokens中
        let mut i = 1;
        loop {
            let t = &tokens[i];
            if t.equal(">") {
                break;
            }
            if t.at_eof() || t.at_eof() {
                error_token!(t, "expected '>'");
            }
            ots.push(t.clone());
            i += 1;
        }

        // 没有引号
        *is_dquote = false;
        // "<"到">"前拼接为字符串
        return join_tokens(&ots);
    }

    // 匹配样式3: #include FOO
    if token.is_ident() {
        // FOO 必须宏展开为单个字符串标记或 "<" ... ">" 序列
        let mut p = Preprocessor::from(processor, &mut tokens);
        let tokens = p.process_without_check();
        // 然后读取引入的文件名
        return read_include_filename(processor, tokens, is_dquote);
    }

    error_token!(token, "expected a filename");
    "".to_string()
}
