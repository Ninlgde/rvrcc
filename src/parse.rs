use crate::{error_token, Node, NodeKind, skip, Token};


pub fn parse(tokens: &Vec<Token>) -> Option<Node> {
    let mut pos = 0;
    let node = expr(&mut pos, tokens);

    let token = &tokens[pos];
    if !token.at_eof() {
        error_token!(token, "extra token");
    }
    node
}

// 解析表达式
// expr = equality
fn expr(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    equality(pos, tokens)
}

// 解析相等性
// equality = relational ("==" relational | "!=" relational)*
fn equality(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // relational
    let mut node = relational(pos, tokens);

    // ("==" relational | "!=" relational)*
    loop {
        let token = &tokens[*pos];
        // "==" relational
        if token.equal("==") {
            *pos += 1;
            let rhs = relational(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdEq, node.unwrap(), rhs));
            continue;
        }

        // "!=" relational
        if token.equal("!=") {
            *pos += 1;
            let rhs = relational(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdNe, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析比较关系
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // add
    let mut node = add(pos, tokens);

    // ("<" add | "<=" add | ">" add | ">=" add)*
    loop {
        let token = &tokens[*pos];
        // "<" add
        if token.equal("<") {
            *pos += 1;
            let rhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLt, node.unwrap(), rhs));
            continue;
        }

        // "<=" add
        if token.equal("<=") {
            *pos += 1;
            let rhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLe, node.unwrap(), rhs));
            continue;
        }

        // ">" add
        // X>Y等价于Y<X
        if token.equal(">") {
            *pos += 1;
            let lhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLt, lhs, node.unwrap()));
            continue;
        }

        // ">=" add
        // X>=Y等价于Y<=X
        if token.equal(">=") {
            *pos += 1;
            let lhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLe, lhs, node.unwrap()));
            continue;
        }

        return node;
    }
}

// 解析加减
// add = mul ("+" mul | "-" mul)*
fn add(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // mul
    let mut node = mul(pos, tokens);

    // ("+" mul | "-" mul)*
    loop {
        let token = &tokens[*pos];
        // "+" mul
        if token.equal("+") {
            *pos += 1;
            let rhs = mul(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdAdd, node.unwrap(), rhs));
            continue;
        }

        // "-" mul
        if token.equal("-") {
            *pos += 1;
            let rhs = mul(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdSub, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析乘除
// mul = unary ("*" unary | "/" unary)*
fn mul(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // unary
    let mut node = unary(pos, tokens);

    // ("*" unary | "/" unary)*
    loop {
        let token = &tokens[*pos];
        // "*" unary
        if token.equal("*") {
            *pos += 1;
            let rhs = unary(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdMul, node.unwrap(), rhs));
            continue;
        }

        // "/" unary
        if token.equal("/") {
            *pos += 1;
            let rhs = unary(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdDiv, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析一元运算
// unary = ("+" | "-") unary | primary
fn unary(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    let token = &tokens[*pos];

    // "+" unary
    if token.equal("+") {
        *pos += 1;
        return unary(pos, tokens);
    }

    // "-" unary
    if token.equal("-") {
        *pos += 1;
        return Some(Node::new_unary(NodeKind::NdNeg, unary(pos, tokens).unwrap()));
    }

    // primary
    primary(pos, tokens)
}

// 解析括号、数字
// primary = "(" expr ")" | num
fn primary(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    let token = &tokens[*pos];
    if token.equal("(") {
        *pos += 1;
        let node = expr(pos, tokens);
        skip(&tokens[*pos], ")", pos);
        return node;
    }
    match token {
        Token::TKNum { val, t_str: _t_str, offset: _offset } => {
            let node = Node::new_num(*val);
            *pos += 1;
            return Some(node);
        }
        _ => {}
    }

    error_token!(&tokens[*pos], "expected an expression");

    None
}