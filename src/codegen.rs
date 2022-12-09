use crate::{Node, NodeKind};

pub fn codegen(mut node: &mut Box<Node>) {
    // 声明一个全局main段，同时也是程序入口段
    print!("  .globl main\n");
    // main段标签
    print!("main:\n");

    // 栈布局
    //-------------------------------// sp
    //              fp                  fp = sp-8
    //-------------------------------// fp
    //              'a'                 fp-8
    //              'b'                 fp-16
    //              ...
    //              'z'                 fp-208
    //-------------------------------// sp=sp-8-208
    //           表达式计算
    //-------------------------------//

    // Prologue, 前言
    // 将fp压入栈中，保存fp的值
    print!("  addi sp, sp, -8\n");
    print!("  sd fp, 0(sp)\n");
    // 将sp写入fp
    print!("  mv fp, sp\n");

    // 26个字母*8字节=208字节，栈腾出208字节的空间
    print!("  addi sp, sp, -208\n");

    loop {
        get_stmt(node);
        if node.next.is_none() { break; }
        node = node.next.as_mut().unwrap();
    }

    // Epilogue，后语
    // 将fp的值改写回sp
    print!("  mv sp, fp\n");
    // 将最早fp保存的值弹栈，恢复fp。
    print!("  ld fp, 0(sp)\n");
    print!("  addi sp, sp, 8\n");
    // 返回
    print!("  ret\n");
}

fn get_stmt(node: &Box<Node>) {
    if node.kind == NodeKind::NdExprStmt {
        let mut depth = 0;
        gen_expr(node.lhs.as_ref().unwrap(), &mut depth);
        assert_eq!(depth, 0);
        return;
    }

    panic!("invalid statement")
}

/// 生成表达式
fn gen_expr(node: &Box<Node>, depth: &mut usize) {
    match node.kind {
        // 加载数字到a0
        NodeKind::NdNum => {
            print!("  li a0, {}\n", node.val);
            return;
        }
        // 对寄存器取反
        NodeKind::NdNeg => {
            gen_expr(node.lhs.as_ref().unwrap(), depth);
            // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
            print!("  neg a0, a0\n");
            return;
        }
        NodeKind::NdVar => {
            // 计算出变量的地址，然后存入a0
            gen_addr(node);
            // 访问a0地址中存储的数据，存入到a0当中
            print!("  ld a0, 0(a0)\n");
            return;
        }
        NodeKind::NdAssign => {
            // 左部是左值，保存值到的地址
            gen_addr(node.lhs.as_ref().unwrap());
            push(depth);
            // 右部是右值，为表达式的值
            gen_expr(node.rhs.as_ref().unwrap(), depth);
            pop("a1", depth);
            print!("  sd a0, 0(a1)\n");
            return;
        }
        _ => {}
    }
    if node.kind == NodeKind::NdNum {
        print!("  li a0, {}\n", node.val);
        return;
    }

    // 递归到最右节点
    gen_expr(node.rhs.as_ref().unwrap(), depth);
    // 将结果压入栈
    push(depth);
    // 递归到左节点
    gen_expr(node.lhs.as_ref().unwrap(), depth);
    // 将结果弹栈到a1
    pop("a1", depth);

    // 生成各个二叉树节点
    match node.kind {
        NodeKind::NdAdd => {
            // + a0=a0+a1
            print!("  add a0, a0, a1\n");
            return;
        }
        NodeKind::NdSub => {
            // - a0=a0-a1
            print!("  sub a0, a0, a1\n");
            return;
        }
        NodeKind::NdMul => {
            // * a0=a0*a1
            print!("  mul a0, a0, a1\n");
            return;
        }
        NodeKind::NdDiv => {
            // / a0=a0/a1
            print!("  div a0, a0, a1\n");
            return;
        }
        NodeKind::NdEq => {
            // a0=a0^a1，异或指令
            print!("  xor a0, a0, a1\n");
            // a0==a1
            // a0=a0^a1, sltiu a0, a0, 1
            // 等于0则置1
            print!("  seqz a0, a0\n");
            return;
        }
        NodeKind::NdNe => {
            // a0=a0^a1，异或指令
            print!("  xor a0, a0, a1\n");
            // a0!=a1
            // a0=a0^a1, sltu a0, x0, a0
            // 不等于0则置1
            print!("  snez a0, a0\n");
            return;
        }
        NodeKind::NdLt => {
            print!("  slt a0, a0, a1\n");
            return;
        }
        NodeKind::NdLe => {
            // a0<=a1等价于
            // a0=a1<a0, a0=a1^1
            print!("  slt a0, a1, a0\n");
            print!("  xori a0, a0, 1\n");
            return;
        }
        _ => {}
    }

    panic!("invalid expression");
}

/// 计算给定节点的绝对地址
/// 如果报错，说明节点不在内存中
fn gen_addr(node: &Box<Node>) {
    if node.kind == NodeKind::NdVar {
        // 偏移量=是两个字母在ASCII码表中的距离加1后乘以8，*8表示每个变量需要八个字节单位的内存
        let offset = ((node.name.as_bytes()[0] - 'a' as u8 + 1) * 8) as i32;
        print!("  addi a0, fp, {}\n", -offset);
        return;
    }

    panic!("not an lvalue")
}

/// 压栈，将结果临时压入栈中备用
/// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
/// 当前栈指针的地址就是sp，将a0的值压入栈
/// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
fn push(depth: &mut usize) {
    print!("  addi sp, sp, -8\n");
    print!("  sd a0, 0(sp)\n");
    *depth += 1;
}

/// 弹栈，将sp指向的地址的值，弹出到a1
fn pop(reg: &str, depth: &mut usize) {
    print!("  ld {}, 0(sp)\n", reg);
    print!("  addi sp, sp, 8\n");
    *depth -= 1;
}
