//! C语言关键字

use lazy_static::lazy_static;
use std::collections::HashSet;
lazy_static! {
/// 关键字列表
    pub static ref KEYWORDS: HashSet<&'static str> = {
        let mut set = HashSet::new();
        set.insert(KW_RETURN);
        set.insert(KW_IF);
        set.insert(KW_ELSE);
        set.insert(KW_FOR);
        set.insert(KW_WHILE);
        set.insert(KW_INT);
        set.insert(KW_SIZEOF);
        set.insert(KW_CHAR);
        set.insert(KW_STRUCT);
        set.insert(KW_UNION);
        set.insert(KW_LONG);
        set.insert(KW_SHORT);
        set.insert(KW_VOID);
        set.insert(KW_TYPEDEF);
        set.insert(KW_BOOL);
        set.insert(KW_ENUM);
        set.insert(KW_STATIC);
        set.insert(KW_GOTO);
        set.insert(KW_BREAK);
        set.insert(KW_CONTINUE);
        set.insert(KW_SWITCH);
        set.insert(KW_CASE);
        set.insert(KW_DEFAULT);
        set.insert(KW_EXTERN);
        set.insert(KW_ALIGNOF);
        set.insert(KW_ALIGNAS);
        set.insert(KW_DO);
        set.insert(KW_SIGNED);
        set.insert(KW_UNSIGNED);
        set.insert(KW_CONST);
        set.insert(KW_VOLATILE);
        set.insert(KW_AUTO);
        set.insert(KW_REGISTER);
        set.insert(KW_RESTRICT);
        set.insert(KW___RESTRICT);
        set.insert(KW___RESTRICT__);
        set.insert(KW_NORETURN);
        set.insert(KW_FLOAT);
        set.insert(KW_DOUBLE);
        set.insert(KW_TYPEOF);
        set.insert(KW_ASM);
        set.insert(KW_INLINE);
        set.insert(KW_THREAD_LOCAL);
        set.insert(KW_THREAD);
        set
    };
}

lazy_static! {
    pub static ref KW_TYPENAME: HashSet<&'static str> = {
        let mut set = HashSet::new();
        set.insert(KW_CHAR);
        set.insert(KW_SHORT);
        set.insert(KW_INT);
        set.insert(KW_LONG);
        set.insert(KW_STRUCT);
        set.insert(KW_UNION);
        set.insert(KW_VOID);
        set.insert(KW_TYPEDEF);
        set.insert(KW_BOOL);
        set.insert(KW_ENUM);
        set.insert(KW_STATIC);
        set.insert(KW_EXTERN);
        set.insert(KW_ALIGNAS);
        set.insert(KW_SIGNED);
        set.insert(KW_UNSIGNED);
        set.insert(KW_CONST);
        set.insert(KW_VOLATILE);
        set.insert(KW_AUTO);
        set.insert(KW_REGISTER);
        set.insert(KW_RESTRICT);
        set.insert(KW___RESTRICT);
        set.insert(KW___RESTRICT__);
        set.insert(KW_NORETURN);
        set.insert(KW_FLOAT);
        set.insert(KW_DOUBLE);
        set.insert(KW_TYPEOF);
        set.insert(KW_INLINE);
        set.insert(KW_THREAD_LOCAL);
        set.insert(KW_THREAD);
        set
    };
}

/// 关键字: 返回
pub const KW_RETURN: &str = "return";
/// 关键字: if
pub const KW_IF: &str = "if";
/// 关键字: else
pub const KW_ELSE: &str = "else";
/// 关键字: for循环
pub const KW_FOR: &str = "for";
/// 关键字: while循环
pub const KW_WHILE: &str = "while";
/// 关键字: int整型
pub const KW_INT: &str = "int";
/// 关键字: sizeof
pub const KW_SIZEOF: &str = "sizeof";
/// 关键字: char字符
pub const KW_CHAR: &str = "char";
/// 关键字: struct结构体
pub const KW_STRUCT: &str = "struct";
/// 关键字: union联合
pub const KW_UNION: &str = "union";
/// 关键字: long长整型
pub const KW_LONG: &str = "long";
/// 关键字: short短整型
pub const KW_SHORT: &str = "short";
/// 关键字: void
pub const KW_VOID: &str = "void";
/// 关键字: typedef
pub const KW_TYPEDEF: &str = "typedef";
/// 关键字: bool
pub const KW_BOOL: &str = "_Bool";
/// 关键字: enum
pub const KW_ENUM: &str = "enum";
/// 关键字: static
pub const KW_STATIC: &str = "static";
/// 关键字: goto
pub const KW_GOTO: &str = "goto";
/// 关键字: break
pub const KW_BREAK: &str = "break";
/// 关键字: continue
pub const KW_CONTINUE: &str = "continue";
/// 关键字: switch
pub const KW_SWITCH: &str = "switch";
/// 关键字: case
pub const KW_CASE: &str = "case";
/// 关键字: default
pub const KW_DEFAULT: &str = "default";
/// 关键字: extern
pub const KW_EXTERN: &str = "extern";
/// 关键字: _Alignof
pub const KW_ALIGNOF: &str = "_Alignof";
/// 关键字: _Alignas
pub const KW_ALIGNAS: &str = "_Alignas";
/// 关键字: do while
pub const KW_DO: &str = "do";
/// 关键字: signed
pub const KW_SIGNED: &str = "signed";
/// 关键字: unsigned
pub const KW_UNSIGNED: &str = "unsigned";
/// 关键字: const
pub const KW_CONST: &str = "const";
/// 关键字: volatile
pub const KW_VOLATILE: &str = "volatile";
/// 关键字: auto
pub const KW_AUTO: &str = "auto";
/// 关键字: register
pub const KW_REGISTER: &str = "register";
/// 关键字: restrict
pub const KW_RESTRICT: &str = "restrict";
/// 关键字: __restrict
pub const KW___RESTRICT: &str = "__restrict";
/// 关键字: __restrict__
pub const KW___RESTRICT__: &str = "__restrict__";
/// 关键字: _Noreturn
pub const KW_NORETURN: &str = "_Noreturn";
/// 关键字: float
pub const KW_FLOAT: &str = "float";
/// 关键字: double
pub const KW_DOUBLE: &str = "double";
/// 关键字: typeof
pub const KW_TYPEOF: &str = "typeof";
/// 关键字: asm
pub const KW_ASM: &str = "asm";
/// 关键字: inline
pub const KW_INLINE: &str = "inline";
/// 关键字: _Thread_local
pub const KW_THREAD_LOCAL: &str = "_Thread_local";
/// 关键字: __thread
pub const KW_THREAD: &str = "__thread";
