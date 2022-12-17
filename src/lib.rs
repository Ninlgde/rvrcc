extern crate core;

mod tokenize;
mod parse;
mod codegen;
mod keywords;
mod ctype;
mod token;
mod obj;
mod node;
mod error_print;
mod utils;

use token::Token;
use obj::Obj;
use ctype::Type;
use node::Node;
use utils::*;

pub use tokenize::tokenize_file;
pub use tokenize::tokenize;
pub use parse::parse;
pub use codegen::codegen;
pub use utils::open_file_for_write;

pub static mut INPUT: String = String::new();
pub static mut FILE_NAME: String = String::new();