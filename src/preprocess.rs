//! 预处理

use crate::token::Token;
use crate::tokenize::convert_keywords;

/// 预处理器入口函数
pub fn preprocess(mut tokens: Vec<Token>) -> Vec<Token> {
    // 将所有关键字的终结符，都标记为KEYWORD
    convert_keywords(&mut tokens);
    tokens
}
