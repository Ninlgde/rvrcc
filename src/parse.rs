//! AST parser
//! program = stmt*
//! stmt = exprStmt
//! exprStmt = expr ";"
//! expr = assign
//! assign = equality ("=" assign)?
//! equality = relational ("==" relational | "!=" relational)*
//! relational = add ("<" add | "<=" add | ">" add | ">=" add)*
//! add = mul ("+" mul | "-" mul)*
//! mul = unary ("*" unary | "/" unary)*
//! unary = ("+" | "-") unary | primary
//! primary = "(" expr ")" | num

use std::slice::Iter;
use std::iter::Peekable;
use crate::{error_token, Function, Node, NodeKind, Var, Token};

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

    pub fn parse(&mut self) -> Function {
        let mut nodes = vec![];

        while let Some(token) = self.peekable.peek() {
            if token.at_eof() {
                break;
            }
            nodes.push(self.stmt().unwrap());
        };

        let offset: isize = ((self.locals.len() + 1) * 8) as isize;
        let program = Function {
            body: nodes,
            locals: self.locals.to_vec(),
            stack_size: align_to(offset, 16),
        };

        return program;
    }

    /// 解析语句
    /// stmt = expr_stmt
    fn stmt(&mut self) -> Option<Node> {
        self.expr_stmt()
    }

    /// 解析表达式语句
    /// expr_stmt = expr ";"
    fn expr_stmt(&mut self) -> Option<Node> {
        let node = Some(Node::new_unary(NodeKind::NdExprStmt, self.expr().unwrap()));
        assert!(self.peekable.peek().unwrap().equal(";"));
        self.peekable.next();
        node
    }

    /// 解析表达式
    /// expr = assign
    fn expr(&mut self) -> Option<Node> {
        self.assign()
    }

    /// 解析赋值
    /// assign = equality ("=" assign)?
    fn assign(&mut self) -> Option<Node> {
        let mut node = self.equality();

        if self.peekable.peek().unwrap().equal("=") {
            self.peekable.next();
            let rhs = self.assign().unwrap();
            node = Some(Node::new_binary(NodeKind::NdAssign, node.unwrap(), rhs));
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
                node = Some(Node::new_binary(NodeKind::NdEq, node.unwrap(), rhs));
                continue;
            }

            // "!=" relational
            if token.equal("!=") {
                self.peekable.next();
                let rhs = self.relational().unwrap();
                node = Some(Node::new_binary(NodeKind::NdNe, node.unwrap(), rhs));
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
                node = Some(Node::new_binary(NodeKind::NdLt, node.unwrap(), rhs));
                continue;
            }

            // "<=" add
            if token.equal("<=") {
                self.peekable.next();
                let rhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::NdLe, node.unwrap(), rhs));
                continue;
            }

            // ">" add
            // X>Y等价于Y<X
            if token.equal(">") {
                self.peekable.next();
                let lhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::NdLt, lhs, node.unwrap()));
                continue;
            }

            // ">=" add
            // X>=Y等价于Y<=X
            if token.equal(">=") {
                self.peekable.next();
                let lhs = self.add().unwrap();
                node = Some(Node::new_binary(NodeKind::NdLe, lhs, node.unwrap()));
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
                node = Some(Node::new_binary(NodeKind::NdAdd, node.unwrap(), rhs));
                continue;
            }

            // "-" mul
            if token.equal("-") {
                self.peekable.next();
                let rhs = self.mul().unwrap();
                node = Some(Node::new_binary(NodeKind::NdSub, node.unwrap(), rhs));
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
                node = Some(Node::new_binary(NodeKind::NdMul, node.unwrap(), rhs));
                continue;
            }

            // "/" unary
            if token.equal("/") {
                self.peekable.next();
                let rhs = self.unary().unwrap();
                node = Some(Node::new_binary(NodeKind::NdDiv, node.unwrap(), rhs));
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
            return Some(Node::new_unary(NodeKind::NdNeg, self.unary().unwrap()));
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
            assert!(self.peekable.peek().unwrap().equal(")"));
            self.peekable.next();
            return node;
        }
        match token {
            Token::TkIdent { t_str, offset: _offset } => {
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
            Token::TKNum { val, t_str: _t_str, offset: _offset } => {
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
}

// 对齐到Align的整数倍
fn align_to(n: isize, align: isize) -> isize {
    // (0,Align]返回Align
    (n + align - 1) / align * align
}