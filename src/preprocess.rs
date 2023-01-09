//! 预处理

use crate::error_token;
use crate::token::Token;
use crate::tokenize::convert_keywords;

/// 预处理器入口函数
pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    // 处理宏和指示
    let mut tokens = preprocess0(tokens);
    // 将所有关键字的终结符，都标记为KEYWORD
    convert_keywords(&mut tokens);
    tokens
}

pub fn preprocess0(tokens: Vec<Token>) -> Vec<Token> {
    let mut new_tokens = Vec::new();
    let mut peekable = tokens.iter().peekable();
    while let Some(token) = peekable.next() {
        if token.is_hash() {
            let t = peekable.next();
            if !t.unwrap().at_bol {
                error_token!(t.unwrap(), "invalid preprocessor directive");
            }
        } else {
            new_tokens.push(token.clone());
        }
    }
    new_tokens
}
