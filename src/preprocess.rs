//! 预处理

use crate::parse::Parser;
use crate::token::Token;
use crate::tokenize::convert_keywords;
use crate::{dirname, error_token, tokenize_file, warn_token};

/// 预处理器入口函数
pub fn preprocess(tokens: &mut Vec<Token>) -> Vec<Token> {
    // 处理宏和指示
    let mut processor = Preprocessor::new(tokens);
    let mut tokens = processor.process();
    // 将所有关键字的终结符，都标记为KEYWORD
    convert_keywords(&mut tokens);
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
}

impl<'a> Preprocessor<'a> {
    /// 构造预处理器
    fn new(tokens: &'a mut Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            result: vec![],
            cond_incls: vec![],
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

    /// 获取当前游标和游标所指的token
    fn current(&self) -> (usize, &Token) {
        (self.cursor, &self.tokens[self.cursor])
    }

    /// 获取当前游标的token
    fn current_token(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    /// 游标指向下一个
    fn next(&mut self) -> &mut Self {
        self.cursor += 1;
        self
    }

    /// 处理
    fn process0(&mut self) {
        while !self.finished() {
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
                let token = self.next().current_token();

                if !token.is_string() {
                    error_token!(token, "expected a filename");
                }

                let filename = token.get_string_literal();
                let path = if filename.starts_with("/") {
                    filename.to_string()
                } else {
                    let dir = dirname(token.get_file_name());
                    format!("{}/{}", dir, filename)
                };
                let include_tokens = tokenize_file(path);
                if include_tokens.len() == 0 {
                    error_token!(token, "include got error");
                }
                // 处理多余的终结符
                self.next().skip_line();
                // 将Tok2接续到Tok->Next的位置
                self.append_tokens(include_tokens);
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

            if token.at_bol() {
                continue;
            }

            error_token!(token, "invalid preprocessor directive");
        }
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
        for token in tokens.into_iter().rev() {
            if !token.at_eof() {
                self.tokens.insert(self.cursor, token);
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
            result.push(self.current_token().clone());
            self.next();
        }

        result.push(Token::new_eof(false, 0, 0));
        result
    }

    /// 读取并计算常量表达式
    fn eval_const_expr(&mut self) -> i64 {
        let (_, start) = self.current();
        let start = start.clone(); // clone走,否则触发借用error
                                   // 解析#if后的常量表达式
        let tokens = self.next().copy_line();
        if tokens.len() <= 1 {
            error_token!(&start, "no expression");
        }

        // 计算常量表达式的值
        let mut parser = Parser::new(&tokens);
        let val = parser.const_expr();
        let (_, pt) = parser.current();
        if !pt.at_eof() {
            error_token!(pt, "extra token");
        }
        val
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
            if token.is_hash() && next.equal("if") {
                self.next().next().skip_cond_incl2();
                continue;
            }
            // #endif
            if token.is_hash() && (next.equal("else") || next.equal("endif")) {
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
            if token.is_hash() && next.equal("if") {
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
}

#[derive(Clone, PartialEq, Eq)]
enum CondInclKind {
    Then,
    Else,
}

/// #if可以嵌套，所以使用栈来保存嵌套的#if
struct CondIncl {
    /// 类型
    ctx: CondInclKind,
    /// 对应的终结符
    token: Token,
    /// 是否被包含
    included: bool,
}
