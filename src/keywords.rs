pub const KEYWORDS: [&str; KW_COUNT] = [
    KW_RETURN, KW_IF, KW_ELSE, KW_FOR, KW_WHILE, KW_INT, KW_SIZEOF, KW_CHAR, KW_STRUCT, KW_UNION,
    KW_LONG, KW_SHORT, KW_VOID, KW_TYPEDEF,
];

const KW_COUNT: usize = 14;

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
