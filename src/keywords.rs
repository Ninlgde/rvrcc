//! C语言关键字

/// 关键字列表
pub const KEYWORDS: [&str; KW_COUNT] = [
    KW_RETURN,
    KW_IF,
    KW_ELSE,
    KW_FOR,
    KW_WHILE,
    KW_INT,
    KW_SIZEOF,
    KW_CHAR,
    KW_STRUCT,
    KW_UNION,
    KW_LONG,
    KW_SHORT,
    KW_VOID,
    KW_TYPEDEF,
    KW_BOOL,
    KW_ENUM,
    KW_STATIC,
    KW_GOTO,
    KW_BREAK,
    KW_CONTINUE,
    KW_SWITCH,
    KW_CASE,
    KW_DEFAULT,
    KW_EXTERN,
    KW_ALIGNOF,
    KW_ALIGNAS,
    KW_DO,
    KW_SIGNED,
];

/// 关键字数量
const KW_COUNT: usize = 28;

/// 类型名称列表
pub const KW_TYPENAME: [&str; KW_TYPENAME_COUNT] = [
    KW_CHAR, KW_SHORT, KW_INT, KW_LONG, KW_STRUCT, KW_UNION, KW_VOID, KW_TYPEDEF, KW_BOOL, KW_ENUM,
    KW_STATIC, KW_EXTERN, KW_ALIGNAS, KW_SIGNED,
];

/// 类型名称数量
const KW_TYPENAME_COUNT: usize = 14;

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
