//! 汇编生成器

use crate::ctype::{TypeKind, TypeLink};
use crate::node::{NodeKind, NodeLink};
use crate::obj::{Obj, ObjLink};
use crate::{align_to, error_token, write_file};
use std::fmt;
use std::io::Write;

/// 形参name
const ARG_NAMES: [&str; 6] = ["a0", "a1", "a2", "a3", "a4", "a5"];

/// 输出文件
static mut OUTPUT: Option<Box<dyn Write>> = None;

/// 汇编代码生成到文件
pub fn codegen(program: &mut Vec<ObjLink>, write_file: Box<dyn Write>) {
    unsafe {
        OUTPUT = Some(write_file);
    }
    let mut generator = Generator::new(program);
    generator.generate();
}

/// 把args输出到OUTPUT
pub fn write2output(args: fmt::Arguments) {
    let output = format!("{}", args);
    unsafe {
        write_file(OUTPUT.as_mut().expect("no output file"), output.as_ref());
    }
}

/// 宏: 输出一行字符串到文件
#[macro_export]
macro_rules! writeln {
    ($fmt: literal $(, $($arg: tt)+)?) => {
        $crate::codegen::write2output(format_args!(concat!($fmt, "\n") $(, $($arg)+)?))
    };
}

/// 宏: 输出字符串到文件
#[macro_export]
macro_rules! write {
    ($fmt: literal $(, $($arg: tt)+)?) => {
        $crate::codegen::write_file(format_args!(concat!($fmt, "") $(, $($arg)+)?))
    };
}

/// 生成器
struct Generator<'a> {
    /// ast
    program: &'a mut Vec<ObjLink>,
    /// 当前生成的方法名
    current_function_name: String,
    /// 栈深
    depth: usize,
    /// 代码段计数
    counter: u32,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a mut Vec<ObjLink>) -> Self {
        Generator {
            program,
            current_function_name: "".to_string(),
            depth: 0,
            counter: 0,
        }
    }

    /// 生成
    pub fn generate(&mut self) {
        self.assign_lvar_offsets();
        self.emit_data();
        self.emit_text();
    }

    /// 给local var赋值offset
    fn assign_lvar_offsets(&mut self) {
        for func in self.program.into_iter() {
            let f = &mut *func.borrow_mut();
            match f {
                Obj::Func {
                    locals, stack_size, ..
                } => {
                    let mut offset = 0;
                    for var in locals.iter().rev() {
                        {
                            let cv = var.clone();
                            let v = cv.borrow();
                            let t = v.get_type().borrow();
                            offset += t.size as isize;
                            offset = align_to(offset, v.get_align());
                        }
                        let mut v = var.borrow_mut();
                        v.set_offset(-offset);
                    }

                    *stack_size = align_to(offset, 16);
                }
                _ => {}
            }
        }
    }

    /// 生成数据段
    fn emit_data(&mut self) {
        for var in self.program.to_vec().iter() {
            let var = &*var.borrow();
            // 跳过无定义的变量
            if !var.is_definition() {
                continue;
            }
            match var {
                Obj::Var {
                    name,
                    type_,
                    init_data,
                    ..
                } => {
                    if var.is_static() {
                        writeln!("\n  # static全局变量{}", var.get_name());
                        writeln!("  .local {}", var.get_name());
                    } else {
                        writeln!("\n  # 全局变量{}", var.get_name());
                        writeln!("  .globl {}", var.get_name());
                    }
                    writeln!("  # 对齐全局变量");
                    if var.get_type().borrow().align == 0 {
                        panic!("Align can not be 0!");
                    }
                    writeln!("  .align {}", simple_log2(var.get_align()));
                    // 判断是否有初始值
                    if init_data.is_some() {
                        writeln!("\n  # 数据段标签");
                        writeln!("  .data");
                        writeln!("{}:", name);
                        let mut rel = var.get_relocation();
                        let mut pos = 0;
                        let chars = init_data.as_ref().unwrap();
                        while pos < var.get_type().borrow().size {
                            unsafe {
                                if !rel.is_null() && (*rel).offset == pos {
                                    // 使用其他变量进行初始化
                                    writeln!("  # {}全局变量", var.get_name());
                                    writeln!("  .quad {}{:+}", &(*rel).label, (*rel).added);
                                    rel = (*rel).next;
                                    pos += 8;
                                } else {
                                    // 打印出字符串的内容，包括转义字符
                                    writeln!("  # 字符串字面量");
                                    let i = chars[pos as usize];
                                    let c = i as u8 as char;
                                    if c.is_ascii() && !c.is_ascii_control() {
                                        writeln!("  .byte {}\t# {}", i, c);
                                    } else {
                                        writeln!("  .byte {}", i);
                                    }
                                    pos += 1;
                                }
                            }
                        }
                        continue;
                    }

                    // bss段未给数据分配空间，只记录数据所需空间的大小
                    writeln!("  # 未初始化的全局变量");
                    writeln!("  .bss");
                    writeln!("{}:", name);
                    writeln!("  # 全局变量零填充{}位", type_.borrow().size);
                    writeln!("  .zero {}", type_.borrow().size);
                }
                _ => {}
            }
        }
    }

    /// 生成代码段
    fn emit_text(&mut self) {
        for function in self.program.to_vec().iter().rev() {
            let function = &*function.borrow_mut();
            match function {
                Obj::Func {
                    name,
                    body,
                    params,
                    stack_size,
                    is_definition,
                    is_static,
                    ..
                } => {
                    if !is_definition {
                        continue;
                    }
                    if *is_static {
                        writeln!("\n  # 定义局部{}函数", name);
                        writeln!("  .local {}", name);
                    } else {
                        writeln!("\n  # 定义全局{}函数", name);
                        writeln!("  .globl {}", name);
                    }

                    writeln!("  # 代码段标签");
                    writeln!("  .text");
                    writeln!("# ====={}段开始===============", name);
                    writeln!("# {}段标签", name);
                    writeln!("{}:", name);
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
                    writeln!("  # 将ra寄存器压栈,保存ra的值");
                    writeln!("  addi sp, sp, -16");
                    writeln!("  sd ra, 8(sp)");
                    // 将fp压入栈中，保存fp的值
                    writeln!("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值");
                    writeln!("  sd fp, 0(sp)");
                    // 将sp写入fp
                    writeln!("  # 将sp的值写入fp");
                    writeln!("  mv fp, sp");

                    // 偏移量为实际变量所用的栈大小
                    writeln!("  # sp腾出StackSize大小的栈空间");
                    writeln!("  addi sp, sp, -{}", stack_size);

                    let mut i = 0;
                    for p in params.iter().rev() {
                        let p = p.borrow();
                        let size = p.get_type().borrow().size;
                        self.store_general(i, p.get_offset(), size);
                        i += 1;
                    }

                    writeln!("# ====={}段主体===============", name);
                    self.gen_stmt(&body.as_ref().unwrap().clone());
                    assert_eq!(self.depth, 0);

                    // Epilogue，后语
                    // 输出return段标签
                    writeln!("# ====={}段结束===============", name);
                    writeln!("# return段标签");
                    writeln!(".L.return.{}:", name);
                    // 将fp的值改写回sp
                    writeln!("  # 将fp的值写回sp");
                    writeln!("  mv sp, fp");
                    // 将最早fp保存的值弹栈，恢复fp。
                    writeln!("  # 将最早fp保存的值弹栈，恢复fp和sp");
                    writeln!("  ld fp, 0(sp)");
                    // 将ra寄存器弹栈,恢复ra的值
                    writeln!("  # 将ra寄存器弹栈,恢复ra的值");
                    writeln!("  ld ra, 8(sp)");
                    writeln!("  addi sp, sp, 16");
                    // 返回
                    writeln!("  # 返回a0值给系统调用");
                    writeln!("  ret");
                }
                _ => {}
            }
        }
    }

    /// 生成语句
    fn gen_stmt(&mut self, node: &NodeLink) {
        writeln!("  .loc 1 {}", node.get_token().get_line_no());

        match node.kind {
            // 生成for或while循环语句
            NodeKind::For => {
                // 代码段计数
                let c: u32 = self.count();
                let brk = node.break_label.as_ref().unwrap();
                let ctn = node.continue_label.as_ref().unwrap();
                writeln!("\n# =====循环语句{}===============", c);
                // 生成初始化语句
                if node.init.is_some() {
                    writeln!("\n# init语句{}", c);
                    self.gen_stmt(node.init.as_ref().unwrap());
                }
                // 输出循环头部标签
                writeln!("\n# 循环{}的.L.begin.{}段标签", c, c);
                writeln!(".L.begin.{}:", c);
                // 处理循环条件语句
                writeln!("# cond表达式{}", c);
                if node.cond.is_some() {
                    // 生成条件循环语句
                    self.gen_expr(node.cond.as_ref().unwrap());
                    // 判断结果是否为0，为0则跳转到结束部分
                    writeln!("  # 若a0为0，则跳转到循环{}的{}段", c, brk);
                    writeln!("  beqz a0, {}", brk);
                }
                // 生成循环体语句
                writeln!("\n# then语句{}", c);
                self.gen_stmt(node.then.as_ref().unwrap());
                // continue标签语句
                writeln!("{}:", ctn);
                // 处理循环递增语句
                if node.inc.is_some() {
                    // 生成循环递增语句
                    writeln!("\n# inc语句{}", c);
                    self.gen_expr(node.inc.as_ref().unwrap());
                }
                // 跳转到循环头部
                writeln!("  # 跳转到循环{}的.L.begin.{}段", c, c);
                writeln!("  j .L.begin.{}", c);
                // 输出循环尾部标签
                writeln!("\n# 循环{}的{}段标签", c, brk);
                writeln!("{}:", brk);
            }
            NodeKind::Do => {
                // 代码段计数
                let c: u32 = self.count();
                let brk = node.break_label.as_ref().unwrap();
                let ctn = node.continue_label.as_ref().unwrap();
                writeln!("\n# =====do while语句语句{}===============", c);
                writeln!("\n# begin语句{}", c);
                writeln!(".L.begin.{}:", c);
                // 生成循环体语句
                writeln!("\n# then语句{}", c);
                self.gen_stmt(node.then.as_ref().unwrap());
                // 处理循环条件语句
                writeln!("\n# Cond语句{}", c);
                writeln!("{}:", ctn);
                self.gen_expr(node.cond.as_ref().unwrap());
                // 跳转到循环头部
                writeln!("  # 跳转到循环{}的.L.begin.{}段", c, c);
                writeln!("  bnez a0, .L.begin.{}", c);
                // 输出循环尾部标签
                writeln!("\n# 循环{}的{}段标签", c, brk);
                writeln!("{}:", brk);
            }
            // 生成if语句
            NodeKind::If => {
                // 代码段计数
                let c: u32 = self.count();
                writeln!("\n# =====分支语句{}==============", c);
                // 生成条件内语句
                writeln!("\n# cond表达式{}", c);
                self.gen_expr(node.cond.as_ref().unwrap());
                // 判断结果是否为0，为0则跳转到else标签
                writeln!("  # 若a0为0，则跳转到分支{}的.L.else.{}段", c, c);
                writeln!("  beqz a0, .L.else.{}", c);
                // 生成符合条件后的语句
                writeln!("\n# then语句{}", c);
                self.gen_stmt(node.then.as_ref().unwrap());
                // 执行完后跳转到if语句后面的语句
                writeln!("  # 跳转到分支{}的.L.end.{}段", c, c);
                writeln!("  j .L.end.{}", c);
                // else代码块，else可能为空，故输出标签
                writeln!("\n# else语句{}", c);
                writeln!("# 分支{}的.L.else.{}段标签", c, c);
                writeln!(".L.else.{}:", c);
                // 生成不符合条件后的语句
                if node.els.is_some() {
                    self.gen_stmt(node.els.as_ref().unwrap());
                }
                // 结束if语句，继续执行后面的语句
                writeln!("\n# 分支{}的.L.end.{}段标签", c, c);
                writeln!(".L.end.{}:", c);
            }
            NodeKind::Switch => {
                writeln!("\n# =====switch语句===============");
                self.gen_expr(node.cond.as_ref().unwrap());

                writeln!("  # 遍历跳转到值等于a0的case标签");
                for case in node.case_next.iter() {
                    writeln!("  li t0, {}", case.val as i32);
                    writeln!("  beq a0, t0, {}", case.continue_label.as_ref().unwrap());
                }

                if node.default_case.is_some() {
                    writeln!("  # 跳转到default标签");
                    writeln!(
                        "  j {}",
                        node.default_case
                            .as_ref()
                            .unwrap()
                            .continue_label
                            .as_ref()
                            .unwrap(),
                    );
                }

                writeln!("  # 结束switch，跳转break标签");
                writeln!("  j {}", node.break_label.as_ref().unwrap());
                // 生成case标签的语句
                self.gen_stmt(node.then.as_ref().unwrap());
                writeln!("# switch的break标签，结束switch");
                writeln!("{}:", node.break_label.as_ref().unwrap());
            }
            NodeKind::Case => {
                writeln!("# case标签，值为{}", node.val as i32);
                writeln!("{}:", node.continue_label.as_ref().unwrap());
                self.gen_stmt(node.lhs.as_ref().unwrap());
            }
            // 生成代码块，遍历代码块的语句vec
            NodeKind::Block => {
                for s in &node.body {
                    self.gen_stmt(s);
                }
            }
            NodeKind::Goto => {
                writeln!("  j {}", node.get_unique_label());
            }
            NodeKind::Label => {
                writeln!("{}:", node.get_unique_label());
                self.gen_stmt(node.lhs.as_ref().unwrap());
            }
            // 生成表达式语句
            NodeKind::ExprStmt => {
                self.gen_expr(node.lhs.as_ref().unwrap());
            }
            // 生成return语句
            NodeKind::Return => {
                writeln!("# 返回语句");
                if node.lhs.is_some() {
                    self.gen_expr(node.lhs.as_ref().unwrap());
                }
                // 无条件跳转语句，跳转到.L.return段
                // j offset是 jal x0, offset的别名指令
                writeln!("  # 跳转到.L.return.{}段", self.current_function_name);
                writeln!("  j .L.return.{}", self.current_function_name);
            }
            _ => {
                error_token!(&node.token, "invalid statement")
            }
        }
    }

    /// 生成表达式
    fn gen_expr(&mut self, node: &NodeLink) {
        // .loc 文件编号 行号
        writeln!("  .loc 1 {}", node.get_token().get_line_no());

        match node.kind {
            NodeKind::NullExpr => {
                return;
            }
            // 加载数字到a0
            NodeKind::Num => {
                writeln!("  # 将{}加载到a0中", node.val);
                writeln!("  li a0, {}", node.val);
                return;
            }
            // 对寄存器取反
            NodeKind::Neg => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
                writeln!("  # 对a0值进行取反");
                writeln!("  neg a0, a0");
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
                    self.gen_stmt(&node.clone());
                }
                return;
            }
            // 逗号
            NodeKind::Comma => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                self.gen_expr(node.rhs.as_ref().unwrap());
                return;
            }
            NodeKind::Cast => {
                let lhs = node.lhs.as_ref().unwrap();
                self.gen_expr(lhs);
                let f_typ = lhs.type_.as_ref().unwrap().clone();
                let t_typ = node.type_.as_ref().unwrap().clone();
                self.cast(f_typ, t_typ);
                return;
            }
            NodeKind::MemZero => {
                let var = node.var.as_ref().unwrap().clone();
                let var = var.borrow();
                let typ = var.get_type().borrow();
                writeln!(
                    "  # 对{}的内存{}(fp)清零{}位",
                    var.get_name(),
                    var.get_offset(),
                    typ.size
                );
                // 对栈内变量所占用的每个字节都进行清零
                for i in 0..typ.size {
                    writeln!("  sb zero, {}(fp)", var.get_offset() + i);
                }
                return;
            }
            NodeKind::Cond => {
                let c: u32 = self.count();
                writeln!("\n# =====条件运算符{}===========", c);
                self.gen_expr(node.cond.as_ref().unwrap());
                writeln!("  # 条件判断，为0则跳转");
                writeln!("  beqz a0, .L.else.{}", c);
                self.gen_expr(node.then.as_ref().unwrap());
                writeln!("  # 跳转到条件运算符结尾部分");
                writeln!("  j .L.end.{}", c);
                writeln!(".L.else.{}:", c);
                self.gen_expr(node.els.as_ref().unwrap());
                writeln!(".L.end.{}:", c);
                return;
            }
            NodeKind::Not => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                writeln!("  # 非运算");
                // a0=0则置1，否则为0
                writeln!("  seqz a0, a0");
                return;
            }
            NodeKind::LogAnd => {
                let c: u32 = self.count();
                writeln!("\n# =====逻辑与{}===============", c);
                self.gen_expr(node.lhs.as_ref().unwrap());
                // 判断是否为短路操作
                writeln!("  # 左部短路操作判断，为0则跳转");
                writeln!("  beqz a0, .L.false.{}", c);
                self.gen_expr(node.rhs.as_ref().unwrap());
                writeln!("  # 右部判断，为0则跳转");
                writeln!("  beqz a0, .L.false.{}", c);
                writeln!("  li a0, 1");
                writeln!("  j .L.end.{}", c);
                writeln!(".L.false.{}:", c);
                writeln!("  li a0, 0");
                writeln!(".L.end.{}:", c);
                return;
            }
            NodeKind::LogOr => {
                let c: u32 = self.count();
                writeln!("\n# =====逻辑或{}===============", c);
                self.gen_expr(node.lhs.as_ref().unwrap());
                // 判断是否为短路操作
                writeln!("  # 左部短路操作判断，不为0则跳转");
                writeln!("  bnez a0, .L.true.{}", c);
                self.gen_expr(node.rhs.as_ref().unwrap());
                writeln!("  # 右部判断，不为0则跳转");
                writeln!("  bnez a0, .L.true.{}", c);
                writeln!("  li a0, 0");
                writeln!("  j .L.end.{}", c);
                writeln!(".L.true.{}:", c);
                writeln!("  li a0, 1");
                writeln!(".L.end.{}:", c);
                return;
            }
            NodeKind::BitNot => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                writeln!("  # 按位取反");
                // 这里的 not a0, a0 为 xori a0, a0, -1 的伪码
                writeln!("  not a0, a0");
                return;
            }
            NodeKind::FuncCall => {
                let mut argc = 0;
                for arg in node.args.iter() {
                    self.gen_expr(arg);
                    self.push();
                    argc += 1;
                }

                // 反向弹栈，a0->参数1，a1->参数2……
                for i in (0..argc).rev() {
                    self.pop(ARG_NAMES[i]);
                }

                if self.depth % 2 == 0 {
                    // 偶数深度，sp已经对齐16字节
                    writeln!("  # 调用{}函数", node.func_name);
                    writeln!("  call {}", node.func_name);
                } else {
                    // 对齐sp到16字节的边界
                    writeln!("  # 对齐sp到16字节的边界，并调用{}函数", node.func_name);
                    writeln!("  addi sp, sp, -8");
                    writeln!("  call {}", node.func_name);
                    writeln!("  addi sp, sp, 8");
                }
                return;
            }
            _ => {}
        }

        self.gen_lrhs(node.lhs.as_ref().unwrap(), node.rhs.as_ref().unwrap());

        let typ = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
        let typ = typ.borrow();
        let suffix = if typ.kind == TypeKind::Long || typ.has_base() {
            ""
        } else {
            "w"
        };
        match node.kind {
            NodeKind::Add => {
                // + a0=a0+a1
                writeln!("  # a0+a1，结果写入a0");
                writeln!("  add{} a0, a0, a1", suffix);
            }
            NodeKind::Sub => {
                // - a0=a0-a1
                writeln!("  # a0-a1，结果写入a0");
                writeln!("  sub{} a0, a0, a1", suffix);
            }
            NodeKind::Mul => {
                // * a0=a0*a1
                writeln!("  # a0×a1，结果写入a0");
                writeln!("  mul{} a0, a0, a1", suffix);
            }
            NodeKind::Div => {
                // / a0=a0/a1
                writeln!("  # a0÷a1，结果写入a0");
                writeln!("  div{} a0, a0, a1", suffix);
            }
            NodeKind::Mod => {
                // % a0=a0%a1
                writeln!("  # a0%%a1，结果写入a0");
                writeln!("  rem{} a0, a0, a1", suffix);
            }
            NodeKind::BitAnd => {
                // & a0=a0&a1
                writeln!("  # a0&a1，结果写入a0");
                writeln!("  and a0, a0, a1");
            }
            NodeKind::BitOr => {
                // | a0=a0|a1
                writeln!("  # a0|a1，结果写入a0");
                writeln!("  or a0, a0, a1");
            }
            NodeKind::BitXor => {
                // ^ a0=a0^a1
                writeln!("  # a0^a1，结果写入a0");
                writeln!("  xor a0, a0, a1");
            }
            NodeKind::Eq => {
                // a0=a0^a1，异或指令
                writeln!("  # 判断是否a0=a1");
                writeln!("  xor a0, a0, a1");
                // a0==a1
                // a0=a0^a1, sltiu a0, a0, 1
                // 等于0则置1
                writeln!("  seqz a0, a0");
            }
            NodeKind::Ne => {
                // a0=a0^a1，异或指令
                writeln!("  # 判断是否a0≠a1");
                writeln!("  xor a0, a0, a1");
                // a0!=a1
                // a0=a0^a1, sltu a0, x0, a0
                // 不等于0则置1
                writeln!("  snez a0, a0");
            }
            NodeKind::Lt => {
                writeln!("  # 判断a0<a1");
                writeln!("  slt a0, a0, a1");
            }
            NodeKind::Le => {
                // a0<=a1等价于
                // a0=a1<a0, a0=a0^1
                writeln!("  # 判断是否a0≤a1");
                writeln!("  slt a0, a1, a0");
                writeln!("  xori a0, a0, 1");
            }
            NodeKind::Shl => {
                writeln!("  # a0逻辑左移a1位");
                writeln!("  sll{} a0, a0, a1", suffix);
            }
            NodeKind::Shr => {
                writeln!("  # a0算术右移a1位");
                writeln!("  sra{} a0, a0, a1", suffix);
            }
            _ => error_token!(&node.get_token(), "invalid expression"),
        }
    }

    /// 生成左右节点的表达式
    fn gen_lrhs(&mut self, lhs: &NodeLink, rhs: &NodeLink) {
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
    fn gen_addr(&mut self, node: &NodeLink) {
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
                        writeln!("  # 获取局部变量{}的栈内地址为{}(fp)", name, offset);
                        writeln!("  addi a0, fp, {}", offset);
                    } else {
                        writeln!("  # 获取全局变量{}的地址", name);
                        writeln!("  la a0, {}", name);
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
            writeln!("  # 计算成员变量的地址偏移量");
            let offset = node.member.as_ref().unwrap().offset;
            writeln!("  li t0, {}", offset);
            writeln!("  add a0, a0, t0");
        } else {
            error_token!(&node.get_token(), "not an lvalue")
        }
    }

    /// 压栈，将结果临时压入栈中备用
    /// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
    /// 当前栈指针的地址就是sp，将a0的值压入栈
    /// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
    fn push(&mut self) {
        writeln!("  # 压栈，将a0的值存入栈顶");
        writeln!("  addi sp, sp, -8");
        writeln!("  sd a0, 0(sp)");
        self.depth += 1;
    }

    /// 弹栈，将sp指向的地址的值，弹出到a1
    fn pop(&mut self, reg: &str) {
        writeln!("  # 弹栈，将栈顶的值存入{}", reg);
        writeln!("  ld {}, 0(sp)", reg);
        writeln!("  addi sp, sp, 8");
        self.depth -= 1;
    }

    /// 加载a0指向的值
    fn load(&mut self, type_: TypeLink) {
        if type_.borrow().kind == TypeKind::Array
            || type_.borrow().kind == TypeKind::Struct
            || type_.borrow().kind == TypeKind::Union
        {
            return;
        }

        writeln!("  # 读取a0中存放的地址，得到的值存入a0");
        let size = type_.borrow().size;
        match size {
            1 => writeln!("  lb a0, 0(a0)"),
            2 => writeln!("  lh a0, 0(a0)"),
            4 => writeln!("  lw a0, 0(a0)"),
            8 => writeln!("  ld a0, 0(a0)"),
            _ => {}
        }
    }

    /// 将栈顶值(为一个地址)存入a0
    fn store(&mut self, type_: TypeLink) {
        self.pop("a1");

        let kind = &type_.borrow().kind;
        if *kind == TypeKind::Struct || *kind == TypeKind::Union {
            let k = if *kind == TypeKind::Struct {
                "结构体"
            } else {
                "联合体"
            };
            writeln!("  # 对{}进行赋值", k);
            for i in 0..type_.borrow().size {
                writeln!("  li t0, {}", i);
                writeln!("  add t0, a0, t0");
                writeln!("  lb t1, 0(t0)");

                writeln!("  li t0, {}", i);
                writeln!("  add t0, a1, t0");
                writeln!("  sb t1, 0(t0)");
            }
            return;
        }

        writeln!("  # 将a0的值，写入到a1中存放的地址");
        let size = type_.borrow().size;
        match size {
            1 => writeln!("  sb a0, 0(a1)"),
            2 => writeln!("  sh a0, 0(a1)"),
            4 => writeln!("  sw a0, 0(a1)"),
            8 => writeln!("  sd a0, 0(a1)"),
            _ => {}
        }
    }

    /// 将整形寄存器的值存入栈中
    fn store_general(&mut self, register: usize, offset: isize, size: isize) {
        writeln!(
            "  # 将{}寄存器的值存入{}(fp)的栈地址",
            ARG_NAMES[register],
            offset
        );
        match size {
            1 => writeln!("  sb {}, {}(fp)", ARG_NAMES[register], offset),
            2 => writeln!("  sh {}, {}(fp)", ARG_NAMES[register], offset),
            4 => writeln!("  sw {}, {}(fp)", ARG_NAMES[register], offset),
            8 => writeln!("  sd {}, {}(fp)", ARG_NAMES[register], offset),
            _ => {
                unreachable!();
            }
        }
    }

    /// 类型转换
    fn cast(&mut self, from: TypeLink, to: TypeLink) {
        if to.borrow().kind == TypeKind::Void {
            return;
        }

        if to.borrow().kind == TypeKind::Bool {
            writeln!("  # 转为bool类型：为0置0，非0置1");
            writeln!("  snez a0, a0");
            return;
        }

        let from_idx = get_type_id(from);
        let to_idx = get_type_id(to);
        let cast = CAST_TABLE[from_idx][to_idx];
        if cast.is_some() {
            writeln!("  # 转换函数");
            writeln!("{}", cast.unwrap());
        }
    }

    /// 代码段计数
    fn count(&mut self) -> u32 {
        let c = self.counter;
        self.counter += 1;
        c
    }
}

// 类型映射表
// 先逻辑左移N位，再算术右移N位，就实现了将64位有符号数转换为64-N位的有符号数
// long 64 -> i8
const L8I1: Option<&str> = Some("  # 转换为i8类型\n  slli a0, a0, 56\n  srai a0, a0, 56");
// long 64 -> i16
const L8I2: Option<&str> = Some("  # 转换为i16类型\n  slli a0, a0, 48\n  srai a0, a0, 48");
// long 64 -> i32
const L8I4: Option<&str> = Some("  # 转换为i32类型\n  slli a0, a0, 32\n  srai a0, a0, 32");

/// 获取类型对应的index
fn get_type_id(typ: TypeLink) -> usize {
    match typ.borrow().kind {
        TypeKind::Char => 0,
        TypeKind::Short => 1,
        TypeKind::Int => 2,
        _ => 3,
    }
}

/// 所有类型转换表
const CAST_TABLE: [[Option<&str>; 10]; 10] = [
    [None, None, None, None, None, None, None, None, None, None],
    [L8I1, None, None, None, None, None, None, None, None, None],
    [L8I1, L8I2, None, None, None, None, None, None, None, None],
    [L8I1, L8I2, L8I4, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None, None, None],
];

/// 返回2^N的N值
fn simple_log2(num: isize) -> isize {
    let mut e = 0;
    let mut n = num;
    while n > 1 {
        if n % 2 == 1 {
            panic!("Wrong value {}", num);
        }
        n /= 2;
        e += 1;
    }
    e
}
