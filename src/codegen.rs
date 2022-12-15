use crate::{error_token, Function, Node};

pub fn codegen(program: &mut Function) {
    // assign_lvar_offsets(program);
    // 声明一个全局main段，同时也是程序入口段
    print!("  # 定义全局main段\n");
    print!("  .globl main\n");
    // main段标签
    print!("\n# =====程序开始===============\n");
    print!("# main段标签，也是程序入口段\n");
    print!("main:\n");

    // 栈布局
    //-------------------------------// sp
    //              ra
    //-------------------------------// ra = sp-8
    //              fp
    //-------------------------------// fp = sp-16
    //             变量
    //-------------------------------// sp = sp-16-StackSize
    //           表达式计算
    //-------------------------------//

    // Prologue, 前言
    // 将ra寄存器压栈,保存ra的值
    print!("  # 将ra寄存器压栈,保存ra的值\n");
    print!("  addi sp, sp, -16\n");
    print!("  sd ra, 8(sp)\n");
    // 将fp压入栈中，保存fp的值
    print!("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值\n");
    print!("  sd fp, 0(sp)\n");
    // 将sp写入fp
    print!("  # 将sp的值写入fp\n");
    print!("  mv fp, sp\n");

    // 偏移量为实际变量所用的栈大小
    print!("  # sp腾出StackSize大小的栈空间\n");
    print!("  addi sp, sp, -{}\n", program.stack_size);

    print!("\n# =====程序主体===============\n");
    gen_stmt(&program.body);

    // Epilogue，后语
    // 输出return段标签
    print!("\n# =====程序结束===============\n");
    print!("# return段标签\n");
    print!(".L.return:\n");
    // 将fp的值改写回sp
    print!("  # 将fp的值写回sp\n");
    print!("  mv sp, fp\n");
    // 将最早fp保存的值弹栈，恢复fp。
    print!("  # 将最早fp保存的值弹栈，恢复fp和sp\n");
    print!("  ld fp, 0(sp)\n");
    // 将ra寄存器弹栈,恢复ra的值
    print!("  # 将ra寄存器弹栈,恢复ra的值\n");
    print!("  ld ra, 8(sp)\n");
    print!("  addi sp, sp, 16\n");
    // 返回
    print!("  # 返回a0值给系统调用\n");
    print!("  ret\n");
}

static mut COUNT: u32 = 1;

fn gen_stmt(node: &Node) {
    let mut depth = 0;
    match node {
        // 生成for或while循环语句
        Node::For { init, inc, cond, then, .. } => {
            // 代码段计数
            let c: u32;
            unsafe {
                c = COUNT;
                COUNT += 1;
            }
            print!("\n# =====循环语句{}===============\n", c);
            // 生成初始化语句
            if init.is_some() {
                print!("\n# init语句{}\n", c);
                gen_stmt(init.as_ref().unwrap());
            }
            // 输出循环头部标签
            print!("\n# 循环{}的.L.begin.{}段标签\n", c, c);
            print!(".L.begin.{}:\n", c);
            // 处理循环条件语句
            print!("# cond表达式{}\n", c);
            if cond.is_some() {
                // 生成条件循环语句
                gen_expr(cond.as_ref().unwrap(), &mut depth);
                // 判断结果是否为0，为0则跳转到结束部分
                print!("  # 若a0为0，则跳转到循环{}的.L.end.{}段\n", c, c);
                print!("  beqz a0, .L.end.{}\n", c);
            }
            // 生成循环体语句
            print!("\n# then语句{}\n", c);
            gen_stmt(then.as_ref().unwrap());
            // 处理循环递增语句
            if inc.is_some() {
                // 生成循环递增语句
                print!("\n# inc语句{}\n", c);
                gen_expr(inc.as_ref().unwrap(), &mut depth);
            }
            // 跳转到循环头部
            print!("  # 跳转到循环{}的.L.begin.{}段\n", c, c);
            print!("  j .L.begin.{}\n", c);
            // 输出循环尾部标签
            print!("\n# 循环{}的.L.end.{}段标签\n", c, c);
            print!(".L.end.{}:\n", c);
        }
        // 生成if语句
        Node::If { cond, then, els, .. } => {
            // 代码段计数
            let c: u32;
            unsafe {
                c = COUNT;
                COUNT += 1;
            }
            print!("\n# =====分支语句{}==============\n", c);
            // 生成条件内语句
            print!("\n# cond表达式{}\n", c);
            gen_expr(cond.as_ref().unwrap(), &mut depth);
            // 判断结果是否为0，为0则跳转到else标签
            print!("  # 若a0为0，则跳转到分支{}的.L.else.{}段\n", c, c);
            print!("  beqz a0, .L.else.{}\n", c);
            // 生成符合条件后的语句
            print!("\n# then语句{}\n", c);
            gen_stmt(then.as_ref().unwrap());
            // 执行完后跳转到if语句后面的语句
            print!("  # 跳转到分支{}的.L.end.{}段\n", c, c);
            print!("  j .L.end.{}\n", c);
            // else代码块，else可能为空，故输出标签
            print!("\n# else语句{}\n", c);
            print!("# 分支{}的.L.else.{}段标签\n", c, c);
            print!(".L.else.{}:\n", c);
            // 生成不符合条件后的语句
            if els.is_some() {
                gen_stmt(els.as_ref().unwrap());
            }
            // 结束if语句，继续执行后面的语句
            print!("\n# 分支{}的.L.end.{}段标签\n", c, c);
            print!(".L.end.{}:\n", c);
        }
        // 生成代码块，遍历代码块的语句vec
        Node::Block { body, .. } => {
            for s in body.iter() {
                gen_stmt(s);
            }
        }
        // 生成表达式语句
        Node::ExprStmt { unary, .. } => {
            gen_expr(unary.as_ref().unwrap(), &mut depth);
        }
        // 生成return语句
        Node::Return { unary, .. } => {
            print!("# 返回语句\n");
            gen_expr(unary.as_ref().unwrap(), &mut depth);
            // 无条件跳转语句，跳转到.L.return段
            // j offset是 jal x0, offset的别名指令
            print!("  # 跳转到.L.return段\n");
            print!("  j .L.return\n");
        }
        _ => {
            error_token!(node.get_token(), "invalid statement")
        }
    }
    assert_eq!(depth, 0);
}

/// 生成表达式
fn gen_expr(node: &Box<Node>, depth: &mut usize) {
    match node.as_ref() {
        Node::Add { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // + a0=a0+a1
            print!("  # a0+a1，结果写入a0\n");
            print!("  add a0, a0, a1\n");
        }
        Node::Sub { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // - a0=a0-a1
            print!("  # a0-a1，结果写入a0\n");
            print!("  sub a0, a0, a1\n");
        }
        Node::Mul { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // * a0=a0*a1
            print!("  # a0×a1，结果写入a0\n");
            print!("  mul a0, a0, a1\n");
        }
        Node::Div { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // / a0=a0/a1
            print!("  # a0÷a1，结果写入a0\n");
            print!("  div a0, a0, a1\n");
        }
        // 对寄存器取反
        Node::Neg { unary, .. } => {
            gen_expr(unary.as_ref().unwrap(), depth);
            // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
            print!("  # 对a0值进行取反\n");
            print!("  neg a0, a0\n");
        }
        Node::Eq { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0=a0^a1，异或指令
            print!("  # 判断是否a0=a1\n");
            print!("  xor a0, a0, a1\n");
            // a0==a1
            // a0=a0^a1, sltiu a0, a0, 1
            // 等于0则置1
            print!("  seqz a0, a0\n");
        }
        Node::Ne { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0=a0^a1，异或指令
            print!("  # 判断是否a0≠a1\n");
            print!("  xor a0, a0, a1\n");
            // a0!=a1
            // a0=a0^a1, sltu a0, x0, a0
            // 不等于0则置1
            print!("  snez a0, a0\n");
        }
        Node::Lt { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            print!("  # 判断a0<a1\n");
            print!("  slt a0, a0, a1\n");
        }
        Node::Le { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0<=a1等价于
            // a0=a1<a0, a0=a0^1
            print!("  # 判断是否a0≤a1\n");
            print!("  slt a0, a1, a0\n");
            print!("  xori a0, a0, 1\n");
        }
        Node::Assign { lhs, rhs, .. } => {
            // 左部是左值，保存值到的地址
            gen_addr(lhs.as_ref().unwrap(), depth);
            push(depth);
            // 右部是右值，为表达式的值
            gen_expr(rhs.as_ref().unwrap(), depth);
            pop("a1", depth);
            print!("  # 将a0的值，写入到a1中存放的地址\n");
            print!("  sd a0, 0(a1)\n");
        }
        Node::FuncCall { func_name, .. } => {
            print!("\n  # 调用函数{}\n", func_name);
            print!("  call {}\n", func_name);
        }
        Node::Addr { unary, .. } => {
            gen_addr(unary.as_ref().unwrap(), depth);
        }
        Node::DeRef { unary, .. } => {
            gen_expr(unary.as_ref().unwrap(), depth);
            print!("  # 读取a0中存放的地址，得到的值存入a0\n");
            print!("  ld a0, 0(a0)\n");
        }
        Node::Var { var: _var, .. } => {
            // 计算出变量的地址，然后存入a0
            gen_addr(node, depth);
            // 访问a0地址中存储的数据，存入到a0当中
            print!("  # 读取a0中存放的地址，得到的值存入a0\n");
            print!("  ld a0, 0(a0)\n");
        }
        // 加载数字到a0
        Node::Num { val, .. } => {
            print!("  # 将{}加载到a0中\n", *val);
            print!("  li a0, {}\n", *val);
        }
        _ => {
            error_token!(node.as_ref().get_token(), "invalid expression")
        }
    }
}

fn gen_lrhs(lhs: &Box<Node>, rhs: &Box<Node>, depth: &mut usize) {
    // 递归到最右节点
    gen_expr(rhs, depth);
    // 将结果压入栈
    push(depth);
    // 递归到左节点
    gen_expr(lhs, depth);
    // 将结果弹栈到a1
    pop("a1", depth);
}

/// 计算给定节点的绝对地址
/// 如果报错，说明节点不在内存中
fn gen_addr(node: &Box<Node>, depth: &mut usize) {
    match node.as_ref() {
        // 变量
        Node::Var { var, .. } => {
            // 偏移量是相对于fp的
            let offset = var.as_ref().unwrap().offset;
            print!("  # 获取变量{}的栈内地址为{}(fp)\n", var.as_ref().unwrap().name,
                   offset);
            print!("  addi a0, fp, {}\n", offset);
        }
        // 解引用*
        Node::DeRef { unary, .. } => {
            gen_expr(unary.as_ref().unwrap(), depth);
        }
        _ => {
            error_token!(node.as_ref().get_token(), "not an lvalue")
        }
    }
}

/// 压栈，将结果临时压入栈中备用
/// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
/// 当前栈指针的地址就是sp，将a0的值压入栈
/// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
fn push(depth: &mut usize) {
    print!("  # 压栈，将a0的值存入栈顶\n");
    print!("  addi sp, sp, -8\n");
    print!("  sd a0, 0(sp)\n");
    *depth += 1;
}

/// 弹栈，将sp指向的地址的值，弹出到a1
fn pop(reg: &str, depth: &mut usize) {
    print!("  # 弹栈，将栈顶的值存入{}\n", reg);
    print!("  ld {}, 0(sp)\n", reg);
    print!("  addi sp, sp, 8\n");
    *depth -= 1;
}
