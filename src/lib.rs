//! c语言编译器
//!

#![deny(missing_docs)]
#![deny(warnings)]

mod codegen;
mod ctype;
mod error_print;
mod initializer;
mod keywords;
mod node;
mod obj;
mod parse;
mod token;
mod tokenize;
mod utils;

use utils::*;

pub use codegen::codegen;
pub use parse::parse;
pub use tokenize::tokenize;
pub use tokenize::tokenize_file;
pub use utils::open_file_for_write;
pub use utils::write_file;

/// 输入(stdin or file)
pub static mut INPUT: String = String::new();

/// 输入文件名称
pub static mut FILE_NAME: String = String::new();
