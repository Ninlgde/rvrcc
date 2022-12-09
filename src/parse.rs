//! AST parser
//! program = "{" compound_stmt
//! compound_stmt = stmt* "}"
//! stmt = "return" expr ";" | "{" compound_stmt | expr_stmt
//! exprStmt = expr ";"
//! expr = assign
//! assign = equality ("=" assign)?
//! equality = relational ("==" relational | "!=" relational)*
//! relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//! add = mul ("+" mul | "-" mul)*
//! mul = unary ("*" unary | "/" unary)*
//! unary = ("+" | "-") unary | primary
//! primary = "(" expr ")" | ident | num

use std::slice::Iter;
use std::iter::Peekable;
use crate::{error_token, Function, Node, NodeKind, Var, Token};
use crate::keywords::KW_RETURN;

pub fn parse(tokens: &Vec<Token>) -> Function {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<'a> {
    peekable: Peekable<Iter<'a, Token>>,
    locals: Vec<Var>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            peekable: tokens.iter().peekable(),
            locals: Vec::new(),
        }
    }

    /// 语法解析入口函数
    /// program = "{" compound_stmt
    pub fn parse(&mut self) -> Function {
        // "{"
        self.skip("{");

        // compound_stmt
        let node = self.compound_stmt().unwrap();

        // 计算栈总深度
        let offset: isize = ((self.locals.len() + 1) * 8) as isize;
        // 构建返回值
        let program = Function {
            body: node,
            locals: self.locals.to_vec(),
            stack_size: align_to(offset, 16),
        };

        program
    }

    /// 解析语句
    /// stmt = "return" expr ";" | "{" compound_stmt | expr_stmt
    fn stmt(&mut self) -> Option<Node> {
        let token = self.peekable.peek().unwrap();
        // "return" expr ";"
        if token.equal(KW_RETURN) {
            self.peekable.next();
            let node = Some(Node::new_unary(NodeKind::Return, self.expr().unwrap()));
            self.skip(";");
            return node;
        }

        if token.equal("{") {
            self.peekable.next();
            return self.compound_stmt();
        }

        // expr_stmt
        self.expr_stmt()
    }

    /// 解析复合语句
    /// compound_stmt = stmt* "}"
    fn compound_stmt(&mut self) -> Option<Node> {
        let mut nodes = vec![];

        // 逐句解析,并压入nodes
        // stmt* "}"
        while let Some(token) = self.peekable.peek() {
            if token.equal("}") {
                break;
            }
            nodes.push(self.stmt().unwrap());
        };

        let mut node = Node::new(NodeKind::Block);
        node.body = nodes;
        self.peekable.next();

        Some(node)
    }

    /// 解析表达式语句
    /// expr_stmt = expr ";"
    fn expr_stmt(&mut self) -> Option<Node> {
        // expr ";"
        let node = Some(Node::new_unary(NodeKind::ExprStmt, self.expr().unwrap()));
        self.skip(";");

        node
    }

    /// 解析表达式
    /// expr = assign
    fn expr(&mut self) -> Option<Node> {
        // assign
        self.assign()
    }

    /// 解析赋值
    /// assign = equality ("=" assign)?
    fn assign(&mut self) -> Option<Node> {
        // equality
        let mut node = self.equality();

        // 可能存在递归赋值，如a=b=1
        // ("=" assign)?
        if self.peekable.peek().unwrap().equal("=") {
            self.peekable.next();
            let rhs = self.assign().unwrap();
            node = Some(Node::new_binary(NodeKind::Assign, node.unwrap(), rhs));
        }

        node
    }

    /// 解析相等性
    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Option<Node> {
        // relational
        let mut node = self.relational();

        // ("==" relational | "!=" relational)*
        loop {
            let token = self.peekable.peek().unwrap();
            // "==" relational
            if token.equal("==") {
                self.peekable.next();
                let rhs = self.relational().unwrap();
                node = Some(Node::new_binary(NodeKind::Eq, node.unwrap(), rhs));
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                self.peekable.next();
                let rhs = self.relational().unwrap();
                node = Some(Node::new_binary(NodeKind::Ne, node.unwrap(), rhs));
                continue;
            }

            return node;
        }
    }

    /// 解析比较关系
    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Option<Node> {
        // add
        let mut node = self.add();

        // ("<" add | "<=" add | ">" add | ">=" add)*
        loop {
            let token = self.peekable.peek().unwrap();
            // "<" add
            if token.equal("<") {
                self.peekable.next();
                let rhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::Lt, node.unwrap(), rhs));
                continue;
            }

            // "<=" add
            if token.equal("<=") {
                self.peekable.next();
                let rhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::Le, node.unwrap(), rhs));
                continue;
            }

            // ">" add
            // X>Y等价于Y<X
            if token.equal(">") {
                self.peekable.next();
                let lhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::Lt, lhs, node.unwrap()));
                continue;
            }

            // ">=" add
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                self.peekable.next();
                let lhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::Le, lhs, node.unwrap()));
                continue;
            }

            return node;
        }
    }

    /// 解析加减
    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Option<Node> {
        // mul
        let mut node = self.mul();

        // ("+" mul | "-" mul)*
        loop {
            let token = self.peekable.peek().unwrap();
            // "+" mul
            if token.equal("+") {
                self.peekable.next();
                let rhs = self.mul().unwrap();
                node = Some(Node::new_binary(NodeKind::Add, node.unwrap(), rhs));
                continue;
            }

            // "-" mul
            if token.equal("-") {
                self.peekable.next();
                let rhs = self.mul().unwrap();
                node = Some(Node::new_binary(NodeKind::Sub, node.unwrap(), rhs));
                continue;
            }

            return node;
        }
    }

    /// 解析乘除
    /// mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Option<Node> {
        // unary
        let mut node = self.unary();

        // ("*" unary | "/" unary)*
        loop {
            let token = self.peekable.peek().unwrap();
            // "*" unary
            if token.equal("*") {
                self.peekable.next();
                let rhs = self.unary().unwrap();
                node = Some(Node::new_binary(NodeKind::Mul, node.unwrap(), rhs));
                continue;
            }

            // "/" unary
            if token.equal("/") {
                self.peekable.next();
                let rhs = self.unary().unwrap();
                node = Some(Node::new_binary(NodeKind::Div, node.unwrap(), rhs));
                continue;
            }

            return node;
        }
    }

    /// 解析一元运算
    /// unary = ("+" | "-") unary | primary
    fn unary(&mut self) -> Option<Node> {
        let token = self.peekable.peek().unwrap();

        // "+" unary
        if token.equal("+") {
            self.peekable.next();
            return self.unary();
        }

        // "-" unary
        if token.equal("-") {
            self.peekable.next();
            return Some(Node::new_unary(NodeKind::Neg, self.unary().unwrap()));
        }

        // primary
        self.primary()
    }

    /// 解析括号、数字、变量
    /// primary = "(" expr ")" | ident｜ num
    fn primary(&mut self) -> Option<Node> {
        let token = self.peekable.peek().unwrap();
        if token.equal("(") {
            self.peekable.next();
            let node = self.expr();
            self.skip(")");
            return node;
        }
        match token {
            Token::Ident { t_str, offset: _offset } => {
                let obj = self.find_var(t_str);
                let node;
                if let Some(var) = obj {
                    node = Node::new_var(Var { name: t_str.to_string(), offset: var.offset });
                } else {
                    let offset: isize = ((self.locals.len() + 1) * 8) as isize;
                    self.locals.push(Var { name: t_str.to_string(), offset });
                    node = Node::new_var(Var { name: t_str.to_string(), offset });
                }
                self.peekable.next();
                return Some(node);
            }
            Token::Num { val, t_str: _t_str, offset: _offset } => {
                let node = Node::new_num(*val);
                self.peekable.next();
                return Some(node);
            }
            _ => {}
        }

        error_token!(self.peekable.peek().unwrap(), "expected an expression");

        None
    }

    fn find_var(&self, name: &String) -> Option<&Var> {
        self.locals.iter().find(|item| { item.name == *name })
    }

    fn skip(&mut self, s: &str) {
        let token = self.peekable.peek().unwrap();
        if !token.equal(s) {
            error_token!(token, "expect '{}'", s);
        }
        self.peekable.next();
    }
}

// 对齐到Align的整数倍
fn align_to(n: isize, align: isize) -> isize {
    // (0,Align]返回Align
    (n + align - 1) / align * align
}