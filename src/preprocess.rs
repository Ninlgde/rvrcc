//! 预处理

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
}

impl<'a> Preprocessor<'a> {
    /// 构造预处理器
    fn new(tokens: &'a mut Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            result: vec![],
        }
    }

    /// 获取当前游标和游标所指的token
    fn current(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    /// 游标指向下一个
    fn next(&mut self) -> &mut Self {
        self.cursor += 1;
        self
    }

    fn finished(&self) -> bool {
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
        let token = self.current();
        if token.at_bol() {
            return;
        }

        warn_token!(token, "extra token");

        while !self.current().at_bol() {
            self.next();
        }
    }

    /// 处理
    fn process(&mut self) -> Vec<Token> {
        while !self.finished() {
            let mut token = self.current();
            if !token.is_hash() {
                // 如果不是#号开头则添加
                self.add_token(token.clone());
                self.next();
                continue;
            }

            token = self.next().current();

            if token.equal("include") {
                token = self.next().current();

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

            if token.at_bol() {
                continue;
            }

            error_token!(token, "invalid preprocessor directive");
        }

        self.result.to_vec()
    }
}
