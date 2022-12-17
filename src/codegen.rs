use std::cell::RefCell;
use std::rc::Rc;
use crate::{error_token, Node, Type, Obj, align_to};

static mut CURRENT_FUNCTION_NAME: String = String::new();

/// 形参name
const ARG_NAMES: [&str; 6] = ["a0", "a1", "a2", "a3", "a4", "a5"];

pub fn codegen(program: &mut Vec<Rc<RefCell<Obj>>>) {
    assign_lvar_offsets(program);
    // 生成数据
    emit_data(program);
    // 生成代码
    emit_text(program);
}

fn assign_lvar_offsets(program: &mut Vec<Rc<RefCell<Obj>>>) {
    for func in program {
        let f = &mut *func.borrow_mut();
        match f {
            Obj::Func { locals, stack_size, .. } => {
                let mut offset = 0;
                for var in locals.iter().rev() {
                    let mut v = var.borrow_mut();
                    offset += v.get_type().get_size() as isize;
                    v.set_offset(-offset);
                }

                *stack_size = align_to(offset, 16);
            }
            _ => {}
        }
    }
}

fn emit_data(program: &mut Vec<Rc<RefCell<Obj>>>) {
    for var in program {
        let var = &*var.borrow();
        match var {
            Obj::Var { name, type_, init_data, .. } => {
                println!("  # 数据段标签");
                println!("  .data");
                // 判断是否有初始值
                if init_data.is_some() {
                    println!("{}:", name);
                    // 打印出字符串的内容，包括转义字符
                    println!("  # 字符串字面量");
                    let chars = init_data.as_ref().unwrap();
                    for i in chars {
                        let c = *i as char;
                        if !c.is_ascii_control() {
                            println!("  .byte {}\t# {}", i, c);
                        } else {
                            println!("  .byte {}", i);
                        }
                    }
                } else {
                    println!("  # 全局段{}", name);
                    println!("  .globl {}", name);
                    println!("{}:", name);
                    println!("  # 全局变量零填充{}位", type_.get_size());
                    println!("  .zero {}", type_.get_size());
                }
            }
            _ => {}
        }
    }
}

fn emit_text(program: &mut Vec<Rc<RefCell<Obj>>>) {
    for function in program.iter().rev() {
        let function = &*function.borrow();
        match function {
            Obj::Func { name, body, params, stack_size, .. } => {
                // 声明一个全局main段，同时也是程序入口段
                println!("\n  # 定义全局{}段", name);
                println!("  .globl {}", name);

                println!("  # 代码段标签");
                println!("  .text");
                println!("# ====={}段开始===============", name);
                println!("# {}段标签", name);
                println!("{}:", name);
                unsafe {
                    CURRENT_FUNCTION_NAME = name.to_string();
                }

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
                println!("  # 将ra寄存器压栈,保存ra的值");
                println!("  addi sp, sp, -16");
                println!("  sd ra, 8(sp)");
                // 将fp压入栈中，保存fp的值
                println!("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值");
                println!("  sd fp, 0(sp)");
                // 将sp写入fp
                println!("  # 将sp的值写入fp");
                println!("  mv fp, sp");

                // 偏移量为实际变量所用的栈大小
                println!("  # sp腾出StackSize大小的栈空间");
                println!("  addi sp, sp, -{}", stack_size);

                let mut i = 0;
                for p in params.iter().rev() {
                    let p = p.borrow();
                    let size = p.get_type().get_size();
                    println!("  # 将{}寄存器的值存入{}的栈地址", ARG_NAMES[i], p.get_name());
                    if size == 1 {
                        println!("  sb {}, {}(fp)", ARG_NAMES[i], p.get_offset());
                    } else {
                        println!("  sd {}, {}(fp)", ARG_NAMES[i], p.get_offset());
                    }
                    i += 1;
                }

                println!("# ====={}段主体===============", name);
                gen_stmt(body.as_ref().unwrap());

                // Epilogue，后语
                // 输出return段标签
                println!("# ====={}段结束===============", name);
                println!("# return段标签");
                println!(".L.return.{}:", name);
                // 将fp的值改写回sp
                println!("  # 将fp的值写回sp");
                println!("  mv sp, fp");
                // 将最早fp保存的值弹栈，恢复fp。
                println!("  # 将最早fp保存的值弹栈，恢复fp和sp");
                println!("  ld fp, 0(sp)");
                // 将ra寄存器弹栈,恢复ra的值
                println!("  # 将ra寄存器弹栈,恢复ra的值");
                println!("  ld ra, 8(sp)");
                println!("  addi sp, sp, 16");
                // 返回
                println!("  # 返回a0值给系统调用");
                println!("  ret");
            }
            _ => {}
        }
    }
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
            println!("\n# =====循环语句{}===============", c);
            // 生成初始化语句
            if init.is_some() {
                println!("\n# init语句{}", c);
                gen_stmt(init.as_ref().unwrap());
            }
            // 输出循环头部标签
            println!("\n# 循环{}的.L.begin.{}段标签", c, c);
            println!(".L.begin.{}:", c);
            // 处理循环条件语句
            println!("# cond表达式{}", c);
            if cond.is_some() {
                // 生成条件循环语句
                gen_expr(cond.as_ref().unwrap(), &mut depth);
                // 判断结果是否为0，为0则跳转到结束部分
                println!("  # 若a0为0，则跳转到循环{}的.L.end.{}段", c, c);
                println!("  beqz a0, .L.end.{}", c);
            }
            // 生成循环体语句
            println!("\n# then语句{}", c);
            gen_stmt(then.as_ref().unwrap());
            // 处理循环递增语句
            if inc.is_some() {
                // 生成循环递增语句
                println!("\n# inc语句{}", c);
                gen_expr(inc.as_ref().unwrap(), &mut depth);
            }
            // 跳转到循环头部
            println!("  # 跳转到循环{}的.L.begin.{}段", c, c);
            println!("  j .L.begin.{}", c);
            // 输出循环尾部标签
            println!("\n# 循环{}的.L.end.{}段标签", c, c);
            println!(".L.end.{}:", c);
        }
        // 生成if语句
        Node::If { cond, then, els, .. } => {
            // 代码段计数
            let c: u32;
            unsafe {
                c = COUNT;
                COUNT += 1;
            }
            println!("\n# =====分支语句{}==============", c);
            // 生成条件内语句
            println!("\n# cond表达式{}", c);
            gen_expr(cond.as_ref().unwrap(), &mut depth);
            // 判断结果是否为0，为0则跳转到else标签
            println!("  # 若a0为0，则跳转到分支{}的.L.else.{}段", c, c);
            println!("  beqz a0, .L.else.{}", c);
            // 生成符合条件后的语句
            println!("\n# then语句{}", c);
            gen_stmt(then.as_ref().unwrap());
            // 执行完后跳转到if语句后面的语句
            println!("  # 跳转到分支{}的.L.end.{}段", c, c);
            println!("  j .L.end.{}", c);
            // else代码块，else可能为空，故输出标签
            println!("\n# else语句{}", c);
            println!("# 分支{}的.L.else.{}段标签", c, c);
            println!(".L.else.{}:", c);
            // 生成不符合条件后的语句
            if els.is_some() {
                gen_stmt(els.as_ref().unwrap());
            }
            // 结束if语句，继续执行后面的语句
            println!("\n# 分支{}的.L.end.{}段标签", c, c);
            println!(".L.end.{}:", c);
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
            println!("# 返回语句");
            gen_expr(unary.as_ref().unwrap(), &mut depth);
            // 无条件跳转语句，跳转到.L.return段
            // j offset是 jal x0, offset的别名指令
            unsafe {
                println!("  # 跳转到.L.return.{}段", CURRENT_FUNCTION_NAME);
                println!("  j .L.return.{}", CURRENT_FUNCTION_NAME);
            }
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
            println!("  # a0+a1，结果写入a0");
            println!("  add a0, a0, a1");
        }
        Node::Sub { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // - a0=a0-a1
            println!("  # a0-a1，结果写入a0");
            println!("  sub a0, a0, a1");
        }
        Node::Mul { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // * a0=a0*a1
            println!("  # a0×a1，结果写入a0");
            println!("  mul a0, a0, a1");
        }
        Node::Div { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // / a0=a0/a1
            println!("  # a0÷a1，结果写入a0");
            println!("  div a0, a0, a1");
        }
        // 对寄存器取反
        Node::Neg { unary, .. } => {
            gen_expr(unary.as_ref().unwrap(), depth);
            // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
            println!("  # 对a0值进行取反");
            println!("  neg a0, a0");
        }
        Node::Eq { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0=a0^a1，异或指令
            println!("  # 判断是否a0=a1");
            println!("  xor a0, a0, a1");
            // a0==a1
            // a0=a0^a1, sltiu a0, a0, 1
            // 等于0则置1
            println!("  seqz a0, a0");
        }
        Node::Ne { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0=a0^a1，异或指令
            println!("  # 判断是否a0≠a1");
            println!("  xor a0, a0, a1");
            // a0!=a1
            // a0=a0^a1, sltu a0, x0, a0
            // 不等于0则置1
            println!("  snez a0, a0");
        }
        Node::Lt { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            println!("  # 判断a0<a1");
            println!("  slt a0, a0, a1");
        }
        Node::Le { lhs, rhs, .. } => {
            gen_lrhs(lhs.as_ref().unwrap(), rhs.as_ref().unwrap(), depth);
            // a0<=a1等价于
            // a0=a1<a0, a0=a0^1
            println!("  # 判断是否a0≤a1");
            println!("  slt a0, a1, a0");
            println!("  xori a0, a0, 1");
        }
        Node::Assign { lhs, rhs, type_, .. } => {
            // 左部是左值，保存值到的地址
            gen_addr(lhs.as_ref().unwrap(), depth);
            push(depth);
            // 右部是右值，为表达式的值
            gen_expr(rhs.as_ref().unwrap(), depth);
            store(type_.as_ref().unwrap().clone(), depth);
        }
        Node::StmtExpr { body, .. } => {
            for node in body {
                gen_stmt(node);
            }
        }
        Node::FuncCall { func_name, args, .. } => {
            let mut argc = 0;
            for arg in args.to_vec() {
                gen_expr(&Box::new(arg), depth);
                push(depth);
                argc += 1;
            }

            // 反向弹栈，a0->参数1，a1->参数2……
            for i in (0..argc).rev() {
                pop(ARG_NAMES[i], depth);
            }

            println!("  # 调用{}函数", func_name);
            println!("  call {}", func_name);
        }
        Node::Addr { unary, .. } => {
            gen_addr(unary.as_ref().unwrap(), depth);
        }
        Node::DeRef { unary, type_, .. } => {
            gen_expr(unary.as_ref().unwrap(), depth);
            load(type_.as_ref().unwrap().clone());
        }
        Node::Var { var: _var, type_, .. } => {
            // 计算出变量的地址，然后存入a0
            gen_addr(node, depth);
            load(type_.as_ref().unwrap().clone());
        }
        // 加载数字到a0
        Node::Num { val, .. } => {
            println!("  # 将{}加载到a0中", *val);
            println!("  li a0, {}", *val);
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
            let var = &*var.as_ref().unwrap().borrow();
            match var {
                Obj::Var { is_local, offset, name, .. } => {
                    if *is_local {
                        // 偏移量是相对于fp的
                        println!("  # 获取局部变量{}的栈内地址为{}(fp)", name,
                               offset);
                        println!("  addi a0, fp, {}", offset);
                    } else {
                        println!("  # 获取全局变量{}的地址", name);
                        println!("  la a0, {}", name);
                    }
                }
                _ => {}
            }
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
    println!("  # 压栈，将a0的值存入栈顶");
    println!("  addi sp, sp, -8");
    println!("  sd a0, 0(sp)");
    *depth += 1;
}

/// 弹栈，将sp指向的地址的值，弹出到a1
fn pop(reg: &str, depth: &mut usize) {
    println!("  # 弹栈，将栈顶的值存入{}", reg);
    println!("  ld {}, 0(sp)", reg);
    println!("  addi sp, sp, 8");
    *depth -= 1;
}

fn load(type_: Box<Type>) {
    match *type_ {
        Type::Array { .. } => {
            return;
        }
        _ => {}
    }

    println!("  # 读取a0中存放的地址，得到的值存入a0");
    let size = type_.get_size();
    if size == 1 {
        println!("  lb a0, 0(a0)");
    } else {
        println!("  ld a0, 0(a0)");
    }
}

fn store(type_: Box<Type>, depth: &mut usize) {
    pop("a1", depth);
    println!("  # 将a0的值，写入到a1中存放的地址");
    let size = type_.get_size();
    if size == 1 {
        println!("  sb a0, 0(a1)");
    } else {
        println!("  sd a0, 0(a1)");
    }
}