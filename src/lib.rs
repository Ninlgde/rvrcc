//! c语言编译器
//!

#![deny(missing_docs)]
#![deny(warnings)]

mod args;
mod cmacro;
mod codegen;
mod ctype;
mod error_print;
mod initializer;
mod keywords;
mod node;
mod obj;
mod parse;
mod preprocess;
mod token;
mod tokenize;
mod utils;

use std::io::Write;
use utils::*;

use crate::token::FileLink;
pub use args::parse_args;
pub use args::Args;
pub use args::TempFile;
pub use args::TempFileCleaner;
pub use codegen::codegen;
pub use parse::parse;
pub use preprocess::preprocess;
pub use tokenize::tokenize_file;
pub use utils::dirname;
pub use utils::file_exists;
pub use utils::find_file;
pub use utils::open_file_for_write;
pub use utils::print_tokens;
pub use utils::replace_extn;
pub use utils::write_file;

/// 输入(stdin or file)
pub static mut INPUT: Option<FileLink> = None;

/// 输出文件
pub static mut OUTPUT: Option<Box<dyn Write>> = None;

/// 输入文件列表
pub static mut INPUTS: Vec<FileLink> = vec![];

/// gp上限
pub const GP_MAX: usize = 8;
/// fp上限
pub const FP_MAX: usize = 8;
