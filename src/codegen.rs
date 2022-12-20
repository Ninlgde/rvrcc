use crate::ctype::TypeKind;
use crate::node::NodeKind;
use crate::{align_to, error_token, Node, Obj, Type};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

/// 形参name
const ARG_NAMES: [&str; 6] = ["a0", "a1", "a2", "a3", "a4", "a5"];

pub fn codegen(program: &mut Vec<Rc<RefCell<Obj>>>, write_file: Box<dyn Write>) {
    let mut generator = Generator::new(program, write_file);
    generator.generate();
}

struct Generator<'a> {
    program: &'a mut Vec<Rc<RefCell<Obj>>>,
    current_function_name: String,
    write_file: Box<dyn Write>,
    depth: usize,
    counter: u32,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a mut Vec<Rc<RefCell<Obj>>>, write_file: Box<dyn Write>) -> Self {
        Generator {
            program,
            current_function_name: "".to_string(),
            write_file,
            depth: 0,
            counter: 0,
        }
    }

    pub fn generate(&mut self) {
        self.assign_lvar_offsets();
        self.emit_data();
        self.emit_text();
    }

    fn write_file(&mut self, mut args: String) {
        args.push('\n');
        self.write_file
            .write(args.as_ref())
            .expect("write file got error");
    }

    fn assign_lvar_offsets(&mut self) {
        for func in self.program.into_iter() {
            let f = &mut *func.borrow_mut();
            match f {
                Obj::Func {
                    locals, stack_size, ..
                } => {
                    let mut offset = 0;
                    for var in locals.iter().rev() {
                        let mut v = var.borrow_mut();
                        offset += v.get_type().get_size() as isize;
                        offset = align_to(offset, v.get_type().align as isize);
                        v.set_offset(-offset);
                    }

                    *stack_size = align_to(offset, 16);
                }
                _ => {}
            }
        }
    }

    fn emit_data(&mut self) {
        for var in self.program.to_vec().iter() {
            let var = &*var.borrow();
            match var {
                Obj::Var {
                    name,
                    type_,
                    init_data,
                    ..
                } => {
                    self.write_file(format!("  # 数据段标签"));
                    self.write_file(format!("  .data"));
                    // 判断是否有初始值
                    if init_data.is_some() {
                        self.write_file(format!("{}:", name));
                        // 打印出字符串的内容，包括转义字符
                        self.write_file(format!("  # 字符串字面量"));
                        let chars = init_data.as_ref().unwrap();
                        for i in chars {
                            let c = *i as char;
                            if !c.is_ascii_control() {
                                self.write_file(format!("  .byte {}\t# {}", i, c));
                            } else {
                                self.write_file(format!("  .byte {}", i));
                            }
                        }
                    } else {
                        self.write_file(format!("  # 全局段{}", name));
                        self.write_file(format!("  .globl {}", name));
                        self.write_file(format!("{}:", name));
                        self.write_file(format!("  # 全局变量零填充{}位", type_.get_size()));
                        self.write_file(format!("  .zero {}", type_.get_size()));
                    }
                }
                _ => {}
            }
        }
    }

    fn emit_text(&mut self) {
        for function in self.program.to_vec().iter().rev() {
            let function = &*function.borrow_mut();
            match function {
                Obj::Func {
                    name,
                    body,
                    params,
                    stack_size,
                    ..
                } => {
                    // 声明一个全局main段，同时也是程序入口段
                    self.write_file(format!("\n  # 定义全局{}段", name));
                    self.write_file(format!("  .globl {}", name));

                    self.write_file(format!("  # 代码段标签"));
                    self.write_file(format!("  .text"));
                    self.write_file(format!("# ====={}段开始===============", name));
                    self.write_file(format!("# {}段标签", name));
                    self.write_file(format!("{}:", name));
                    self.current_function_name = name.to_string();

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
                    self.write_file(format!("  # 将ra寄存器压栈,保存ra的值"));
                    self.write_file(format!("  addi sp, sp, -16"));
                    self.write_file(format!("  sd ra, 8(sp)"));
                    // 将fp压入栈中，保存fp的值
                    self.write_file(format!(
                        "  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值"
                    ));
                    self.write_file(format!("  sd fp, 0(sp)"));
                    // 将sp写入fp
                    self.write_file(format!("  # 将sp的值写入fp"));
                    self.write_file(format!("  mv fp, sp"));

                    // 偏移量为实际变量所用的栈大小
                    self.write_file(format!("  # sp腾出StackSize大小的栈空间"));
                    self.write_file(format!("  addi sp, sp, -{}", stack_size));

                    let mut i = 0;
                    for p in params.iter().rev() {
                        let p = p.borrow();
                        let size = p.get_type().get_size();
                        self.store_general(i, p.get_offset(), size);
                        i += 1;
                    }

                    self.write_file(format!("# ====={}段主体===============", name));
                    self.gen_stmt(&Box::new(body.as_ref().unwrap().clone()));
                    assert_eq!(self.depth, 0);

                    // Epilogue，后语
                    // 输出return段标签
                    self.write_file(format!("# ====={}段结束===============", name));
                    self.write_file(format!("# return段标签"));
                    self.write_file(format!(".L.return.{}:", name));
                    // 将fp的值改写回sp
                    self.write_file(format!("  # 将fp的值写回sp"));
                    self.write_file(format!("  mv sp, fp"));
                    // 将最早fp保存的值弹栈，恢复fp。
                    self.write_file(format!("  # 将最早fp保存的值弹栈，恢复fp和sp"));
                    self.write_file(format!("  ld fp, 0(sp)"));
                    // 将ra寄存器弹栈,恢复ra的值
                    self.write_file(format!("  # 将ra寄存器弹栈,恢复ra的值"));
                    self.write_file(format!("  ld ra, 8(sp)"));
                    self.write_file(format!("  addi sp, sp, 16"));
                    // 返回
                    self.write_file(format!("  # 返回a0值给系统调用"));
                    self.write_file(format!("  ret"));
                }
                _ => {}
            }
        }
    }

    fn gen_stmt(&mut self, node: &Box<Node>) {
        self.write_file(format!("  .loc 1 {}", node.get_token().get_line_no()));

        match node.kind {
            // 生成for或while循环语句
            NodeKind::For => {
                // 代码段计数
                let c: u32 = self.counter;
                self.counter += 1;
                self.write_file(format!("\n# =====循环语句{}===============", c));
                // 生成初始化语句
                if node.init.is_some() {
                    self.write_file(format!("\n# init语句{}", c));
                    self.gen_stmt(node.init.as_ref().unwrap());
                }
                // 输出循环头部标签
                self.write_file(format!("\n# 循环{}的.L.begin.{}段标签", c, c));
                self.write_file(format!(".L.begin.{}:", c));
                // 处理循环条件语句
                self.write_file(format!("# cond表达式{}", c));
                if node.cond.is_some() {
                    // 生成条件循环语句
                    self.gen_expr(node.cond.as_ref().unwrap());
                    // 判断结果是否为0，为0则跳转到结束部分
                    self.write_file(format!("  # 若a0为0，则跳转到循环{}的.L.end.{}段", c, c));
                    self.write_file(format!("  beqz a0, .L.end.{}", c));
                }
                // 生成循环体语句
                self.write_file(format!("\n# then语句{}", c));
                self.gen_stmt(node.then.as_ref().unwrap());
                // 处理循环递增语句
                if node.inc.is_some() {
                    // 生成循环递增语句
                    self.write_file(format!("\n# inc语句{}", c));
                    self.gen_expr(node.inc.as_ref().unwrap());
                }
                // 跳转到循环头部
                self.write_file(format!("  # 跳转到循环{}的.L.begin.{}段", c, c));
                self.write_file(format!("  j .L.begin.{}", c));
                // 输出循环尾部标签
                self.write_file(format!("\n# 循环{}的.L.end.{}段标签", c, c));
                self.write_file(format!(".L.end.{}:", c));
            }
            // 生成if语句
            NodeKind::If => {
                // 代码段计数
                let c: u32 = self.counter;
                self.counter += 1;
                self.write_file(format!("\n# =====分支语句{}==============", c));
                // 生成条件内语句
                self.write_file(format!("\n# cond表达式{}", c));
                self.gen_expr(node.cond.as_ref().unwrap());
                // 判断结果是否为0，为0则跳转到else标签
                self.write_file(format!("  # 若a0为0，则跳转到分支{}的.L.else.{}段", c, c));
                self.write_file(format!("  beqz a0, .L.else.{}", c));
                // 生成符合条件后的语句
                self.write_file(format!("\n# then语句{}", c));
                self.gen_stmt(node.then.as_ref().unwrap());
                // 执行完后跳转到if语句后面的语句
                self.write_file(format!("  # 跳转到分支{}的.L.end.{}段", c, c));
                self.write_file(format!("  j .L.end.{}", c));
                // else代码块，else可能为空，故输出标签
                self.write_file(format!("\n# else语句{}", c));
                self.write_file(format!("# 分支{}的.L.else.{}段标签", c, c));
                self.write_file(format!(".L.else.{}:", c));
                // 生成不符合条件后的语句
                if node.els.is_some() {
                    self.gen_stmt(node.els.as_ref().unwrap());
                }
                // 结束if语句，继续执行后面的语句
                self.write_file(format!("\n# 分支{}的.L.end.{}段标签", c, c));
                self.write_file(format!(".L.end.{}:", c));
            }
            // 生成代码块，遍历代码块的语句vec
            NodeKind::Block => {
                for s in &node.body {
                    self.gen_stmt(&Box::new(s.clone()));
                }
            }
            // 生成表达式语句
            NodeKind::ExprStmt => {
                self.gen_expr(node.lhs.as_ref().unwrap());
            }
            // 生成return语句
            NodeKind::Return => {
                self.write_file(format!("# 返回语句"));
                self.gen_expr(node.lhs.as_ref().unwrap());
                // 无条件跳转语句，跳转到.L.return段
                // j offset是 jal x0, offset的别名指令
                self.write_file(format!(
                    "  # 跳转到.L.return.{}段",
                    self.current_function_name
                ));
                self.write_file(format!("  j .L.return.{}", self.current_function_name));
            }
            _ => {
                error_token!(&node.token, "invalid statement")
            }
        }
    }

    /// 生成表达式
    fn gen_expr(&mut self, node: &Box<Node>) {
        // .loc 文件编号 行号
        self.write_file(format!("  .loc 1 {}", node.get_token().get_line_no()));

        match node.kind {
            // 加载数字到a0
            NodeKind::Num => {
                self.write_file(format!("  # 将{}加载到a0中", node.val));
                self.write_file(format!("  li a0, {}", node.val));
                return;
            }
            // 对寄存器取反
            NodeKind::Neg => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
                self.write_file(format!("  # 对a0值进行取反"));
                self.write_file(format!("  neg a0, a0"));
                return;
            }
            // 变量
            NodeKind::Var | NodeKind::Member => {
                // 计算出变量的地址，然后存入a0
                self.gen_addr(node);
                self.load(node.type_.as_ref().unwrap().clone());
                return;
            }
            // 解引用
            NodeKind::DeRef => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                self.load(node.type_.as_ref().unwrap().clone());
                return;
            }
            // 取地址
            NodeKind::Addr => {
                self.gen_addr(node.lhs.as_ref().unwrap());
                return;
            }
            // 赋值
            NodeKind::Assign => {
                // 左部是左值，保存值到的地址
                self.gen_addr(node.lhs.as_ref().unwrap());
                self.push();
                // 右部是右值，为表达式的值
                self.gen_expr(node.rhs.as_ref().unwrap());
                self.store(node.type_.as_ref().unwrap().clone());
                return;
            }
            // 语句表达式
            NodeKind::StmtExpr => {
                for node in &node.body {
                    self.gen_stmt(&Box::new(node.clone()));
                }
                return;
            }
            // 逗号
            NodeKind::Comma => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                self.gen_expr(node.rhs.as_ref().unwrap());
                return;
            }
            NodeKind::FuncCall => {
                let mut argc = 0;
                for arg in node.args.to_vec() {
                    self.gen_expr(&Box::new(arg));
                    self.push();
                    argc += 1;
                }

                // 反向弹栈，a0->参数1，a1->参数2……
                for i in (0..argc).rev() {
                    self.pop(ARG_NAMES[i]);
                }

                self.write_file(format!("  # 调用{}函数", node.func_name));
                self.write_file(format!("  call {}", node.func_name));
                return;
            }
            _ => {}
        }

        self.gen_lrhs(node.lhs.as_ref().unwrap(), node.rhs.as_ref().unwrap());

        match node.kind {
            NodeKind::Add => {
                // + a0=a0+a1
                self.write_file(format!("  # a0+a1，结果写入a0"));
                self.write_file(format!("  add a0, a0, a1"));
                return;
            }
            NodeKind::Sub => {
                // - a0=a0-a1
                self.write_file(format!("  # a0-a1，结果写入a0"));
                self.write_file(format!("  sub a0, a0, a1"));
                return;
            }
            NodeKind::Mul => {
                // * a0=a0*a1
                self.write_file(format!("  # a0×a1，结果写入a0"));
                self.write_file(format!("  mul a0, a0, a1"));
                return;
            }
            NodeKind::Div => {
                // / a0=a0/a1
                self.write_file(format!("  # a0÷a1，结果写入a0"));
                self.write_file(format!("  div a0, a0, a1"));
                return;
            }
            NodeKind::Eq => {
                // a0=a0^a1，异或指令
                self.write_file(format!("  # 判断是否a0=a1"));
                self.write_file(format!("  xor a0, a0, a1"));
                // a0==a1
                // a0=a0^a1, sltiu a0, a0, 1
                // 等于0则置1
                self.write_file(format!("  seqz a0, a0"));
                return;
            }
            NodeKind::Ne => {
                // a0=a0^a1，异或指令
                self.write_file(format!("  # 判断是否a0≠a1"));
                self.write_file(format!("  xor a0, a0, a1"));
                // a0!=a1
                // a0=a0^a1, sltu a0, x0, a0
                // 不等于0则置1
                self.write_file(format!("  snez a0, a0"));
                return;
            }
            NodeKind::Lt => {
                self.write_file(format!("  # 判断a0<a1"));
                self.write_file(format!("  slt a0, a0, a1"));
                return;
            }
            NodeKind::Le => {
                // a0<=a1等价于
                // a0=a1<a0, a0=a0^1
                self.write_file(format!("  # 判断是否a0≤a1"));
                self.write_file(format!("  slt a0, a1, a0"));
                self.write_file(format!("  xori a0, a0, 1"));
                return;
            }
            _ => {}
        }

        error_token!(node.as_ref().get_token(), "invalid expression")
    }

    fn gen_lrhs(&mut self, lhs: &Box<Node>, rhs: &Box<Node>) {
        // 递归到最右节点
        self.gen_expr(rhs);
        // 将结果压入栈
        self.push();
        // 递归到左节点
        self.gen_expr(lhs);
        // 将结果弹栈到a1
        self.pop("a1");
    }
    /// 计算给定节点的绝对地址
    /// 如果报错，说明节点不在内存中
    fn gen_addr(&mut self, node: &Box<Node>) {
        if node.kind == NodeKind::Var {
            // 变量
            let var = &node.var;
            let var = &*var.as_ref().unwrap().borrow();
            match var {
                Obj::Var {
                    is_local,
                    offset,
                    name,
                    ..
                } => {
                    if *is_local {
                        // 偏移量是相对于fp的
                        self.write_file(format!(
                            "  # 获取局部变量{}的栈内地址为{}(fp)",
                            name, offset
                        ));
                        self.write_file(format!("  addi a0, fp, {}", offset));
                    } else {
                        self.write_file(format!("  # 获取全局变量{}的地址", name));
                        self.write_file(format!("  la a0, {}", name));
                    }
                }
                _ => {}
            }
        } else if node.kind == NodeKind::DeRef {
            // 解引用*
            self.gen_expr(node.lhs.as_ref().unwrap());
        } else if node.kind == NodeKind::Comma {
            // 逗号
            self.gen_expr(node.lhs.as_ref().unwrap());
            self.gen_addr(node.rhs.as_ref().unwrap());
        } else if node.kind == NodeKind::Member {
            // 逗号
            self.gen_addr(node.lhs.as_ref().unwrap());
            self.write_file(format!("  # 计算成员变量的地址偏移量"));
            let offset = node.member.as_ref().unwrap().offset;
            self.write_file(format!("  li t0, {}", offset));
            self.write_file(format!("  add a0, a0, t0"));
        } else {
            error_token!(node.as_ref().get_token(), "not an lvalue")
        }
    }

    /// 压栈，将结果临时压入栈中备用
    /// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
    /// 当前栈指针的地址就是sp，将a0的值压入栈
    /// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
    fn push(&mut self) {
        self.write_file(format!("  # 压栈，将a0的值存入栈顶"));
        self.write_file(format!("  addi sp, sp, -8"));
        self.write_file(format!("  sd a0, 0(sp)"));
        self.depth += 1;
    }

    /// 弹栈，将sp指向的地址的值，弹出到a1
    fn pop(&mut self, reg: &str) {
        self.write_file(format!("  # 弹栈，将栈顶的值存入{}", reg));
        self.write_file(format!("  ld {}, 0(sp)", reg));
        self.write_file(format!("  addi sp, sp, 8"));
        self.depth -= 1;
    }

    fn load(&mut self, type_: Box<Type>) {
        if type_.kind == TypeKind::Array
            || type_.kind == TypeKind::Struct
            || type_.kind == TypeKind::Union
        {
            return;
        }

        self.write_file(format!("  # 读取a0中存放的地址，得到的值存入a0"));
        let size = type_.get_size();
        if size == 1 {
            self.write_file(format!("  lb a0, 0(a0)"));
        } else if size == 4 {
            self.write_file(format!("  lw a0, 0(a0)"));
        } else {
            self.write_file(format!("  ld a0, 0(a0)"));
        }
    }

    fn store(&mut self, type_: Box<Type>) {
        self.pop("a1");

        if type_.kind == TypeKind::Struct || type_.kind == TypeKind::Union {
            let kind = if type_.kind == TypeKind::Struct {
                "结构体"
            } else {
                "联合体"
            };
            self.write_file(format!("  # 对{}进行赋值", kind));
            for i in 0..type_.size {
                self.write_file(format!("  li t0, {}", i));
                self.write_file(format!("  add t0, a0, t0"));
                self.write_file(format!("  lb t1, 0(t0)"));

                self.write_file(format!("  li t0, {}", i));
                self.write_file(format!("  add t0, a1, t0"));
                self.write_file(format!("  sb t1, 0(t0)"));
            }
            return;
        }

        self.write_file(format!("  # 将a0的值，写入到a1中存放的地址"));
        let size = type_.get_size();
        if size == 1 {
            self.write_file(format!("  sb a0, 0(a1)"));
        } else if size == 4 {
            self.write_file(format!("  sw a0, 0(a1)"));
        } else {
            self.write_file(format!("  sd a0, 0(a1)"));
        }
    }

    fn store_general(&mut self, register: usize, offset: isize, size: usize) {
        self.write_file(format!(
            "  # 将{}寄存器的值存入{}(fp)的栈地址",
            ARG_NAMES[register], offset
        ));
        match size {
            1 => self.write_file(format!("  sb {}, {}(fp)", ARG_NAMES[register], offset)),
            4 => self.write_file(format!("  sw {}, {}(fp)", ARG_NAMES[register], offset)),
            8 => self.write_file(format!("  sd {}, {}(fp)", ARG_NAMES[register], offset)),
            _ => {
                unreachable!();
            }
        }
    }
}
