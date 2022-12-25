extern crate core;

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

use ctype::*;
use node::*;
use obj::*;
use token::Token;
use utils::*;

pub use codegen::codegen;
pub use parse::parse;
pub use tokenize::tokenize;
pub use tokenize::tokenize_file;
pub use utils::open_file_for_write;

pub static mut INPUT: String = String::new();
pub static mut FILE_NAME: String = String::new();
