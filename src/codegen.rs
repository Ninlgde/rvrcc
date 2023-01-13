//! 汇编生成器

use crate::ctype::{cal_flo_st_mems_ty, TypeKind, TypeLink};
use crate::node::{NodeKind, NodeLink};
use crate::obj::{Obj, ObjLink};
use crate::{align_to, error_token, write_file, FP_MAX, GP_MAX, INPUTS};
use std::fmt;
use std::io::Write;

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
    /// 大结构体的深度
    bsdepth: usize,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a mut Vec<ObjLink>) -> Self {
        Generator {
            program,
            current_function_name: "".to_string(),
            depth: 0,
            counter: 0,
            bsdepth: 0,
        }
    }

    /// 生成
    pub fn generate(&mut self) {
        unsafe {
            for input in INPUTS.iter() {
                writeln!("  .file {} \"{}\"", input.borrow().no, input.borrow().name);
            }
        }
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
                    locals,
                    stack_size,
                    params,
                    ..
                } => {
                    // 反向偏移量
                    let mut re_offset = 16;

                    // 被调用函数将自己的ra、fp也压入栈了，
                    // 所以fp+16才是上一级函数的sp顶
                    // /             栈保存的N个变量            / N*8
                    // /---------------本级函数----------------/ sp
                    // /                 ra                  / sp-8
                    // /                fp（上一级）           / fp = sp-16

                    // 寄存器传递
                    let mut gp = 0;
                    let mut fp = 0;
                    // 寄存器传递的参数
                    for param in params.iter().rev() {
                        let mut var = param.borrow_mut();
                        let typ = var.get_type().clone();
                        let size = typ.borrow().size;
                        if typ.borrow().is_float() {
                            if fp < FP_MAX {
                                writeln!(" #  FP{}传递浮点变量{}", fp, var.get_name());
                                fp += 1;
                                continue;
                            } else if gp < GP_MAX {
                                writeln!(" #  GP{}传递浮点变量{}", gp, var.get_name());
                                gp += 1;
                                continue;
                            }
                        } else {
                            if gp < GP_MAX {
                                writeln!(" #  GP{}传递整型变量{}", gp, var.get_name());
                                gp += 1;
                                continue;
                            }
                        }
                        // 栈传递
                        // 对齐变量
                        re_offset = align_to(re_offset, 8);
                        // 为栈传递变量赋一个偏移量，或者说是反向栈地址
                        var.set_offset(re_offset);
                        // 栈传递变量计算反向偏移量
                        re_offset += size;
                        writeln!(" #  栈传递变量{}偏移量{}", var.get_name(), var.get_offset());
                    }

                    let mut offset = 0;
                    for var in locals.iter().rev() {
                        {
                            let cv = var.clone();
                            let v = cv.borrow();
                            if v.get_offset() != 0 {
                                continue;
                            }
                            let t = v.get_type().borrow();
                            offset += t.size as isize;
                            offset = align_to(offset, v.get_align());
                        }
                        let mut v = var.borrow_mut();
                        v.set_offset(-offset);
                        writeln!(" #  寄存器传递变量{}偏移量{}", v.get_name(), v.get_offset());
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
                    typ,
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
                    writeln!("  # 全局变量零填充{}位", typ.borrow().size);
                    writeln!("  .zero {}", typ.borrow().size);
                }
                _ => {}
            }
        }
    }

    /// 生成代码段
    fn emit_text(&mut self) {
        for function in self.program.to_vec().iter() {
            let function = &*function.borrow();
            match function {
                Obj::Func {
                    name,
                    body,
                    params,
                    stack_size,
                    is_definition,
                    is_static,
                    va_area,
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
                    writeln!("  li t0, -{}", stack_size);
                    writeln!("  add sp, sp, t0");

                    // 正常传递的形参
                    let mut gp = 0;
                    let mut fp = 0;
                    for p in params.iter().rev() {
                        let p = p.borrow();
                        if p.get_offset() > 0 {
                            continue;
                        }
                        let is_float = p.get_type().borrow().is_float();
                        let size = p.get_type().borrow().size;
                        if is_float {
                            if fp < FP_MAX {
                                writeln!("  # 将浮点形参{}的寄存器fa{}的值压栈", p.get_name(), fp);
                                self.store_float(fp, p.get_offset(), size);
                                fp += 1;
                            } else {
                                writeln!("  # 将浮点形参{}的寄存器a{}的值压栈", p.get_name(), gp);
                                self.store_general(gp, p.get_offset(), size);
                                gp += 1;
                            }
                        } else {
                            writeln!("  # 将整型形参{}的寄存器a{}的值压栈", p.get_name(), gp);
                            self.store_general(gp, p.get_offset(), size);
                            gp += 1;
                        }
                    }

                    // 可变参数
                    if va_area.is_some() {
                        // 可变参数存入__va_area__，注意最多为7个
                        let va_area = va_area.as_ref().unwrap().borrow();
                        let mut offset = va_area.get_offset();
                        while gp < GP_MAX {
                            writeln!(
                                "  # 可变参数，相对{}的偏移量为{}",
                                va_area.get_name(),
                                offset - va_area.get_offset()
                            );
                            self.store_general(gp, offset, 8);
                            gp += 1;
                            offset += 8;
                        }
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
        let token = node.get_token();
        writeln!("  .loc {} {}", token.get_file_no(), token.get_line_no());

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
                    self.not_zero(node.cond.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
                self.not_zero(node.cond.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
                self.not_zero(node.cond.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
        let token = node.get_token();
        writeln!("  .loc {} {}", token.get_file_no(), token.get_line_no());

        match node.kind {
            NodeKind::NullExpr => {
                return;
            }
            // 加载数字到a0
            NodeKind::Num => {
                let typ = node.typ.as_ref().unwrap().borrow();
                if typ.kind == TypeKind::Float {
                    writeln!("  # 将a0转换到float类型值为{0:.6}的fa0中", node.fval);

                    writeln!(
                        "  li a0, {}  # float {1:.6}",
                        (node.fval as f32).to_bits(),
                        node.fval
                    );
                    writeln!("  fmv.w.x fa0, a0");
                } else if typ.kind == TypeKind::Double {
                    writeln!("  # 将a0转换到double类型值为{0:.6}的fa0中", node.fval);
                    writeln!(
                        "  li a0, {}  # double {1:.6}",
                        node.fval.to_bits(),
                        node.fval
                    );
                    writeln!("  fmv.d.x fa0, a0");
                } else {
                    writeln!("  # 将{}加载到a0中", node.val);
                    writeln!("  li a0, {}", node.val);
                }
                return;
            }
            // 对寄存器取反
            NodeKind::Neg => {
                // 计算左部的表达式
                self.gen_expr(node.lhs.as_ref().unwrap());

                let typ = node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone();
                let typ = typ.borrow();
                match typ.kind {
                    TypeKind::Float => {
                        writeln!("  # 对float类型的fa0值进行取反");
                        writeln!("  fneg.s fa0, fa0");
                    }
                    TypeKind::Double => {
                        writeln!("  # 对double类型的fa0值进行取反");
                        writeln!("  fneg.d fa0, fa0");
                    }
                    _ => {
                        // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
                        writeln!("  # 对a0值进行取反");
                        writeln!("  neg a0, a0");
                    }
                }
                return;
            }
            // 变量
            NodeKind::Var | NodeKind::Member => {
                // 计算出变量的地址，然后存入a0
                self.gen_addr(node);
                self.load(node.typ.as_ref().unwrap().clone());
                return;
            }
            // 解引用
            NodeKind::DeRef => {
                self.gen_expr(node.lhs.as_ref().unwrap());
                self.load(node.typ.as_ref().unwrap().clone());
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
                self.store(node.typ.as_ref().unwrap().clone());
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
                let f_typ = lhs.typ.as_ref().unwrap().clone();
                let t_typ = node.typ.as_ref().unwrap().clone();
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
                    writeln!("  li t0, {}", var.get_offset() + i);
                    writeln!("  add t0, fp, t0");
                    writeln!("  sb zero, 0(t0)");
                }
                return;
            }
            NodeKind::Cond => {
                let c: u32 = self.count();
                writeln!("\n# =====条件运算符{}===========", c);
                self.gen_expr(node.cond.as_ref().unwrap());
                self.not_zero(node.cond.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
                self.not_zero(node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone());
                writeln!("  # 非运算");
                // a0=0则置1，否则为0
                writeln!("  seqz a0, a0");
                return;
            }
            NodeKind::LogAnd => {
                let c: u32 = self.count();
                writeln!("\n# =====逻辑与{}===============", c);
                self.gen_expr(node.lhs.as_ref().unwrap());
                self.not_zero(node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone());
                // 判断是否为短路操作
                writeln!("  # 左部短路操作判断，为0则跳转");
                writeln!("  beqz a0, .L.false.{}", c);
                self.gen_expr(node.rhs.as_ref().unwrap());
                self.not_zero(node.rhs.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
                self.not_zero(node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone());
                // 判断是否为短路操作
                writeln!("  # 左部短路操作判断，不为0则跳转");
                writeln!("  bnez a0, .L.true.{}", c);
                self.gen_expr(node.rhs.as_ref().unwrap());
                self.not_zero(node.rhs.as_ref().unwrap().typ.as_ref().unwrap().clone());
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
                // 计算所有参数的值，正向压栈
                // 此处获取到栈传递参数的数量
                let mut args = node.args.to_vec();
                let stack = self.push_args(&mut args);
                self.gen_expr(node.lhs.as_ref().unwrap());
                // 将a0的值存入t0
                writeln!("  mv t0, a0");

                // 反向弹栈，a0->参数1，a1->参数2……
                let mut gp = 0;
                let mut fp = 0;
                let mut pi = 0;

                let func_type = node.func_type.as_ref().unwrap().clone();
                let params = &func_type.borrow().params;
                for arg in node.args.iter() {
                    // 如果是可变参数函数
                    // 匹配到空参数（最后一个）的时候，将剩余的整型寄存器弹栈
                    if func_type.borrow().is_variadic && pi >= params.len() {
                        if gp < GP_MAX {
                            writeln!("  # a{}传递可变实参", gp);
                            self.pop(gp);
                            gp += 1;
                        }
                        continue;
                    }
                    pi += 1;
                    let typ = arg.typ.as_ref().unwrap();
                    let typ = typ.borrow();
                    match typ.kind {
                        TypeKind::Struct | TypeKind::Union => {
                            // 判断结构体的类型
                            // 结构体的大小
                            let ts = typ.size;
                            let fs_reg1ty = typ.fs_reg1ty.as_ref().unwrap();
                            let fs_reg2ty = typ.fs_reg2ty.as_ref().unwrap();

                            eprintln!(
                                "function call oo depth = {}, step = {}, {}, {}",
                                self.depth,
                                -1,
                                fs_reg1ty.borrow().is_float(),
                                fs_reg2ty.borrow().is_float()
                            );
                            // 处理一或两个浮点成员变量的结构体
                            if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
                                let regs = vec![fs_reg1ty.clone(), fs_reg2ty.clone()];
                                for (i, reg) in regs.iter().enumerate() {
                                    let reg = reg.borrow();
                                    if reg.kind == TypeKind::Float {
                                        writeln!("  # {}字节float结构体{}通过fa{}传递", ts, i, fp);
                                        writeln!("  # 弹栈，将栈顶的值存入fa{}", fp);
                                        writeln!("  flw fa{}, 0(sp)", fp);
                                        writeln!("  addi sp, sp, 8");
                                        fp += 1;
                                        self.depth -= 1;
                                        eprintln!(
                                            "function call depth = {}, step = {}, {}, {}",
                                            self.depth,
                                            -1,
                                            fs_reg1ty.borrow().is_float(),
                                            fs_reg2ty.borrow().is_float()
                                        );
                                    }
                                    if reg.kind == TypeKind::Double {
                                        writeln!("  # {}字节double结构体{}通过fa{}传递", ts, i, fp);
                                        self.pop_float(fp);
                                        fp += 1;
                                    }
                                    if reg.is_int() {
                                        writeln!("  # {}字节浮点结构体{}通过a{}传递", ts, i, gp);
                                        self.pop(gp);
                                        gp += 1;
                                    }
                                }
                            } else {
                                // 其他整型结构体或多字节结构体
                                // 9~16字节整型结构体用两个寄存器，其他字节结构体用一个结构体
                                let reg = if 8 < ts && ts <= 16 { 2 } else { 1 };
                                for i in 1..reg + 1 {
                                    if gp < GP_MAX {
                                        writeln!("  # {}字节的整型结构体{}通过a{}传递", ts, i, gp);
                                        self.pop(gp);
                                        gp += 1;
                                    }
                                }
                            }
                        }
                        TypeKind::Float | TypeKind::Double => {
                            if fp < FP_MAX {
                                writeln!("  # fa{}传递浮点参数", fp);
                                self.pop_float(fp);
                                fp += 1;
                            } else if gp < GP_MAX {
                                writeln!("  # a{}传递浮点参数", gp);
                                self.pop(gp);
                                gp += 1;
                            }
                        }
                        _ => {
                            if gp < GP_MAX {
                                writeln!("  # a{}传递整型参数", gp);
                                self.pop(gp);
                                gp += 1;
                            }
                        }
                    }
                }

                // 偶数深度，sp已经对齐16字节
                writeln!("  # 调用函数");
                writeln!("  jalr t0");

                // 回收为栈传递的变量开辟的栈空间
                if stack > 0 {
                    // 栈的深度减去栈传递参数的字节数
                    self.depth -= stack;
                    eprintln!("stack call depth = {}, step = -{}", self.depth, stack);
                    writeln!("  # 回收栈传递参数的{}个字节", stack * 8);
                    writeln!("  addi sp, sp, {}", stack * 8);
                    // 清除记录的大结构体的数量
                    self.bsdepth = 0;
                }

                // 清除寄存器中高位无关的数据
                match node.typ.as_ref().unwrap().borrow().kind {
                    TypeKind::Bool => {
                        writeln!("  # 清除bool类型的高位");
                        writeln!("  slli a0, a0, 63");
                        writeln!("  srli a0, a0, 63");
                    }
                    TypeKind::Char => {
                        writeln!("  # 清除char类型的高位");
                        let is_unsigned = node.typ.as_ref().unwrap().borrow().is_unsigned;
                        if is_unsigned {
                            writeln!("  slli a0, a0, 56");
                            writeln!("  srli a0, a0, 56");
                        } else {
                            writeln!("  slli a0, a0, 56");
                            writeln!("  srai a0, a0, 56");
                        }
                    }
                    TypeKind::Short => {
                        writeln!("  # 清除short类型的高位");
                        let is_unsigned = node.typ.as_ref().unwrap().borrow().is_unsigned;
                        if is_unsigned {
                            writeln!("  slli a0, a0, 48");
                            writeln!("  srli a0, a0, 48");
                        } else {
                            writeln!("  slli a0, a0, 48");
                            writeln!("  srai a0, a0, 48");
                        }
                    }
                    _ => {}
                }
                return;
            }
            _ => {}
        }
        let typ = node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone();
        let typ = typ.borrow();

        // 处理浮点类型
        if typ.is_float() {
            self.gen_float(node);
            return;
        }

        self.gen_lrhs(node.lhs.as_ref().unwrap(), node.rhs.as_ref().unwrap());

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
                let is_unsigned = node.typ.as_ref().unwrap().borrow().is_unsigned;
                if is_unsigned {
                    writeln!("  divu{} a0, a0, a1", suffix);
                } else {
                    writeln!("  div{} a0, a0, a1", suffix);
                }
            }
            NodeKind::Mod => {
                // % a0=a0%a1
                writeln!("  # a0%%a1，结果写入a0");
                let is_unsigned = node.typ.as_ref().unwrap().borrow().is_unsigned;
                if is_unsigned {
                    writeln!("  remu{} a0, a0, a1", suffix);
                } else {
                    writeln!("  rem{} a0, a0, a1", suffix);
                }
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
                let is_unsigned = typ.is_unsigned;
                if is_unsigned {
                    writeln!("  sltu a0, a0, a1");
                } else {
                    writeln!("  slt a0, a0, a1");
                }
            }
            NodeKind::Le => {
                // a0<=a1等价于
                // a0=a1<a0, a0=a0^1
                writeln!("  # 判断是否a0≤a1");
                let is_unsigned = typ.is_unsigned;
                if is_unsigned {
                    writeln!("  sltu a0, a1, a0");
                } else {
                    writeln!("  slt a0, a1, a0");
                }
                writeln!("  xori a0, a0, 1");
            }
            NodeKind::Shl => {
                writeln!("  # a0逻辑左移a1位");
                writeln!("  sll{} a0, a0, a1", suffix);
            }
            NodeKind::Shr => {
                writeln!("  # a0算术右移a1位");
                let is_unsigned = node.typ.as_ref().unwrap().borrow().is_unsigned;
                if is_unsigned {
                    writeln!("  srl{} a0, a0, a1", suffix);
                } else {
                    writeln!("  sra{} a0, a0, a1", suffix);
                }
            }
            _ => error_token!(&node.get_token(), "invalid expression"),
        }
    }

    fn gen_float(&mut self, node: &NodeLink) {
        // 递归到最右节点
        self.gen_expr(node.rhs.as_ref().unwrap());
        // 将结果压入栈
        self.push_float();
        // 递归到左节点
        self.gen_expr(node.lhs.as_ref().unwrap());
        // 将结果弹栈到a1
        self.pop_float(1);

        // 生成各个二叉树节点
        // float对应s(single)后缀，double对应d(double)后缀
        let typ = node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone();
        let typ = typ.borrow();
        let suffix = if typ.kind == TypeKind::Float {
            "s"
        } else {
            "d"
        };

        match node.kind {
            NodeKind::Add => {
                writeln!("  # fa0+fa1，结果写入fa0");
                writeln!("  fadd.{} fa0, fa0, fa1", suffix);
            }
            NodeKind::Sub => {
                writeln!("  # fa0-fa1，结果写入fa0");
                writeln!("  fsub.{} fa0, fa0, fa1", suffix);
            }
            NodeKind::Mul => {
                writeln!("  # fa0×fa1，结果写入fa0");
                writeln!("  fmul.{} fa0, fa0, fa1", suffix);
            }
            NodeKind::Div => {
                writeln!("  # fa0÷fa1，结果写入fa0");
                writeln!("  fdiv.{} fa0, fa0, fa1", suffix);
            }
            NodeKind::Eq => {
                writeln!("  # 判断是否fa0=fa1");
                writeln!("  feq.{} a0, fa0, fa1", suffix);
            }
            NodeKind::Ne => {
                writeln!("  # 判断是否fa0≠fa1");
                writeln!("  feq.{} a0, fa0, fa1", suffix);
                writeln!("  seqz a0, a0");
            }
            NodeKind::Lt => {
                writeln!("  # 判断是否fa0<fa1");
                writeln!("  flt.{} a0, fa0, fa1", suffix);
            }
            NodeKind::Le => {
                writeln!("  # 判断是否fa0≤fa1");
                writeln!("  fle.{} a0, fa0, fa1", suffix);
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
        self.pop(1);
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
                        writeln!("  li t0, {}", offset);
                        writeln!("  add a0, fp, t0");
                    } else {
                        let typ = node.typ.as_ref().unwrap();
                        if typ.borrow().kind == TypeKind::Func {
                            writeln!("  # 获取函数{}的地址", name);
                        } else {
                            writeln!("  # 获取全局变量{}的地址", name);
                        }
                        writeln!("  la a0, {}", name);
                    }
                }
                Obj::Func { name, .. } => {
                    let typ = node.typ.as_ref().unwrap();
                    if typ.borrow().kind == TypeKind::Func {
                        writeln!("  # 获取函数{}的地址", name);
                    } else {
                        writeln!("  # 获取全局变量{}的地址", name);
                    }
                    writeln!("  la a0, {}", name);
                }
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

    /// 将函数实参计算后压入栈中
    fn push_args0(&mut self, args: &Vec<NodeLink>, first_pass: bool) {
        if args.len() == 0 {
            return;
        }

        for arg in args.iter().rev() {
            // 第一遍对栈传递的变量进行压栈
            // 第二遍对寄存器传递的变量进行压栈
            if (first_pass && !arg.pass_by_stack) || (!first_pass && arg.pass_by_stack) {
                continue;
            }
            writeln!("\n  # ↓对表达式进行计算，然后压栈↓");
            // 计算出表达式
            self.gen_expr(arg);
            let typ = arg.typ.as_ref().unwrap();
            let at = arg.typ.as_ref().unwrap().borrow();
            match at.kind {
                TypeKind::Struct | TypeKind::Union => self.push_struct(typ.clone()),
                TypeKind::Float | TypeKind::Double => {
                    self.push_float();
                }
                _ => {
                    self.push();
                }
            }
            writeln!("  # ↑结束压栈↑");
        }
    }

    /// 处理参数后进行压栈
    fn push_args(&mut self, args: &mut Vec<NodeLink>) -> usize {
        let mut stack = 0;
        let mut fp = 0;
        let mut gp = 0;

        // 遍历所有参数，优先使用寄存器传递，然后是栈传递
        for arg in args.iter_mut() {
            let at = arg.typ.as_ref().unwrap().clone();
            let kind = at.borrow().kind.clone();
            match kind {
                TypeKind::Struct | TypeKind::Union => {
                    // 判断结构体的类型
                    let tys = cal_flo_st_mems_ty(at.clone(), gp, fp);
                    // 结构体的大小
                    let mut typ = at.borrow_mut();
                    typ.fs_reg1ty = Some(tys[0].clone());
                    typ.fs_reg2ty = Some(tys[1].clone());
                    eprintln!(
                        "push_args {} {}",
                        tys[0].borrow().is_float(),
                        tys[1].borrow().is_float()
                    );
                    let ts = typ.size;
                    let fs_reg1ty = tys[0].clone();
                    let fs_reg2ty = tys[1].clone();
                    if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
                        for reg in tys.iter() {
                            if reg.borrow().is_float() {
                                fp += 1;
                            }
                            if reg.borrow().is_int() {
                                gp += 1;
                            }
                        }
                    } else {
                        // 9~16字节整型结构体用两个寄存器，其他字节结构体用一个寄存器
                        let reg = if 8 < ts && ts <= 16 { 2 } else { 1 };
                        for _ in 1..reg + 1 {
                            if gp < GP_MAX {
                                gp += 1;
                            } else {
                                arg.pass_by_stack = true;
                                stack += 1;
                            }
                        }
                    }
                }
                TypeKind::Float | TypeKind::Double => {
                    // 浮点优先使用FP，而后是GP，最后是栈传递
                    if fp < FP_MAX {
                        writeln!("  # 浮点{}值通过fa{}传递", arg.fval, fp);
                        fp += 1;
                    } else if gp < GP_MAX {
                        writeln!("  # 浮点{}值通过a{}传递", arg.fval, gp);
                        gp += 1;
                    } else {
                        writeln!("  # 浮点{}值通过栈传递", arg.fval);
                        arg.pass_by_stack = true;
                        stack += 1;
                    }
                }
                _ => {
                    // 整型优先使用GP，最后是栈传递
                    if gp < GP_MAX {
                        writeln!("  # 整型{}值通过a{}传递", arg.val, gp);
                        gp += 1;
                    } else {
                        writeln!("  # 整型{}值通过栈传递", arg.val);
                        arg.pass_by_stack = true;
                        stack += 1;
                    }
                }
            }
        }

        // 对齐栈边界
        if (self.depth + stack) % 2 == 1 {
            writeln!("  # 对齐栈边界到16字节");
            writeln!("  addi sp, sp, -8");
            self.depth += 1;
            eprintln!("对齐栈边界 depth = {}, step = {}", self.depth, 1);
            stack += 1;
        }

        // 进行压栈
        // 开辟大于16字节的结构体的栈空间
        let bsstack = self.create_bsspace(args);
        // 第一遍对栈传递的变量进行压栈
        self.push_args0(args, true);
        // 第二遍对寄存器传递的变量进行压栈
        self.push_args0(args, false);
        // 返回栈传递参数的个数
        stack + bsstack
    }

    /// 为大结构体开辟空间
    fn create_bsspace(&mut self, args: &Vec<NodeLink>) -> usize {
        let mut bsstack = 0usize;
        for arg in args.iter() {
            let at = arg.typ.as_ref().unwrap();
            let typ = at.borrow();
            if typ.size > 16 && typ.kind == TypeKind::Struct {
                writeln!("\n  # 大于16字节的结构体，先开辟相应的栈空间\n");
                let size = align_to(typ.size, 8) as usize;
                writeln!("  addi sp, sp, -{}", size);
                self.depth += size / 8;
                eprintln!("create_bsspace depth = {}, step = {}", self.depth, size / 8);
                bsstack += size / 8;
            }
        }
        bsstack
    }

    /// 传递结构体的指针
    fn push_struct(&mut self, typ: TypeLink) {
        let t = typ.borrow();
        // 大于16字节的结构体
        if t.size > 16 {
            // 将结构体复制一份到栈中，然后通过寄存器或栈传递被复制结构体的地址
            // ---------------------------------
            //             大结构体      ←
            // ---------------------------------
            //      栈传递的   其他变量
            // ---------------------------------
            //            大结构体的指针  ↑
            // ---------------------------------

            // 计算大结构体的偏移量
            let size = align_to(t.size, 8) as usize;
            let bsoffset = (self.depth + self.bsdepth) * 8;
            self.bsdepth += size / 8;

            writeln!("  # 复制{}字节的，结构体到{}(sp)的位置", size, bsoffset);
            for i in 0..size {
                writeln!("  lb t0, {}(a0)", i);
                writeln!("  sb t0, {}(sp)", i + bsoffset);
            }

            writeln!("  # 大于16字节的结构体，对结构体地址压栈");
            writeln!("  addi a0, sp, {}", bsoffset);
            self.push();
            return;
        }

        // 含有两个成员（含浮点）的结构体
        // 展开到栈内的两个8字节的空间
        let fs_reg1ty = t.fs_reg1ty.as_ref().unwrap();
        let fs_reg2ty = t.fs_reg2ty.as_ref().unwrap();
        if (fs_reg1ty.borrow().is_float() && fs_reg2ty.borrow().kind != TypeKind::Void)
            || fs_reg2ty.borrow().is_float()
        {
            writeln!("  # 对含有两个成员（含浮点）结构体进行压栈");
            writeln!("  addi sp, sp, -16");
            self.depth += 2;
            eprintln!(
                "push_struct1 depth = {}, step = {} {} {}",
                self.depth,
                2,
                fs_reg1ty.borrow().is_float(),
                fs_reg2ty.borrow().is_float()
            );

            writeln!("  ld t0, 0(a0)");
            writeln!("  sd t0, 0(sp)");

            // 计算第二部分在结构体中的偏移量
            writeln!("  ld t0, {}(a0)", fs_reg2ty.borrow().size);
            writeln!("  sd t0, 8(sp)");

            return;
        }

        // 处理只有一个浮点成员的结构体
        // 或者是小于16字节的结构体
        let s = if fs_reg1ty.borrow().is_float() {
            "只有一个浮点"
        } else {
            "小于16字节"
        };
        let size = align_to(t.size, 8) as usize;
        writeln!("  # 为{}的结构体开辟{}字节的空间，", s, size);
        writeln!("  addi sp, sp, -{}", size);
        self.depth += size / 8;
        eprintln!("push_struct2 depth = {}, step = {}", self.depth, size / 8);

        writeln!("  # 开辟{}字节的空间，复制{}的内存", size, s);
        for i in 0..t.size {
            writeln!("  lb t0, {}(a0)", i);
            writeln!("  sb t0, {}(sp)", i);
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
        eprintln!("push depth = {}, step = {}", self.depth, 1);
    }

    /// 弹栈，将sp指向的地址的值，弹出到a1
    fn pop(&mut self, reg: usize) {
        writeln!("  # 弹栈，将栈顶的值存入a{}", reg);
        writeln!("  ld a{}, 0(sp)", reg);
        writeln!("  addi sp, sp, 8");
        self.depth -= 1;
        eprintln!("pop depth = {}, step = {}", self.depth, -1);
    }

    /// 对于浮点类型进行压栈
    fn push_float(&mut self) {
        writeln!("  # 压栈，将fa0的值存入栈顶");
        writeln!("  addi sp, sp, -8");
        writeln!("  fsd fa0, 0(sp)");
        self.depth += 1;
        eprintln!("push_float depth = {}, step = {}", self.depth, 1);
    }

    /// 对于浮点类型进行弹栈
    fn pop_float(&mut self, reg: usize) {
        writeln!("  # 弹栈，将栈顶的值存入fa{}", reg);
        writeln!("  fld fa{}, 0(sp)", reg);
        writeln!("  addi sp, sp, 8");
        self.depth -= 1;
        eprintln!("pop_float depth = {}, step = {}", self.depth, -1);
    }

    /// 加载a0指向的值
    fn load(&mut self, typ: TypeLink) {
        if typ.borrow().kind == TypeKind::Array
            || typ.borrow().kind == TypeKind::Struct
            || typ.borrow().kind == TypeKind::Union
            || typ.borrow().kind == TypeKind::Func
        {
            return;
        } else if typ.borrow().kind == TypeKind::Float {
            writeln!("  # 访问a0中存放的地址，取得的值存入fa0");
            writeln!("  flw fa0, 0(a0)");
            return;
        } else if typ.borrow().kind == TypeKind::Double {
            writeln!("  # 访问a0中存放的地址，取得的值存入fa0");
            writeln!("  fld fa0, 0(a0)");
            return;
        }

        writeln!("  # 读取a0中存放的地址，得到的值存入a0");
        let size = typ.borrow().size;
        let suffix = if typ.borrow().is_unsigned { "u" } else { "" };
        match size {
            1 => writeln!("  lb{} a0, 0(a0)", suffix),
            2 => writeln!("  lh{} a0, 0(a0)", suffix),
            4 => writeln!("  lw{} a0, 0(a0)", suffix),
            _ => writeln!("  ld a0, 0(a0)"),
        }
    }

    /// 将栈顶值(为一个地址)存入a0
    fn store(&mut self, typ: TypeLink) {
        self.pop(1);

        let kind = &typ.borrow().kind;
        if typ.borrow().is_struct_union() {
            let k = if *kind == TypeKind::Struct {
                "结构体"
            } else {
                "联合体"
            };
            writeln!("  # 对{}进行赋值", k);
            for i in 0..typ.borrow().size {
                writeln!("  li t0, {}", i);
                writeln!("  add t0, a0, t0");
                writeln!("  lb t1, 0(t0)");

                writeln!("  li t0, {}", i);
                writeln!("  add t0, a1, t0");
                writeln!("  sb t1, 0(t0)");
            }
            return;
        } else if *kind == TypeKind::Float {
            writeln!("  # 将fa0的值，写入到a1中存放的地址");
            writeln!("  fsw fa0, 0(a1)");
            return;
        } else if *kind == TypeKind::Double {
            writeln!("  # 将fa0的值，写入到a1中存放的地址");
            writeln!("  fsd fa0, 0(a1)");
            return;
        }

        writeln!("  # 将a0的值，写入到a1中存放的地址");
        let size = typ.borrow().size;
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
        writeln!("  # 将a{}寄存器的值存入{}(fp)的栈地址", register, offset);
        writeln!("  li t0, {}", offset);
        writeln!("  add t0, fp, t0");
        match size {
            1 => writeln!("  sb a{}, 0(t0)", register),
            2 => writeln!("  sh a{}, 0(t0)", register),
            4 => writeln!("  sw a{}, 0(t0)", register),
            8 => writeln!("  sd a{}, 0(t0)", register),
            _ => {
                unreachable!();
            }
        }
    }

    /// 将浮点寄存器的值存入栈中
    fn store_float(&mut self, register: usize, offset: isize, size: isize) {
        writeln!("  # 将fa{}寄存器的值存入{}(fp)的栈地址", register, offset);
        writeln!("  li t0, {}", offset);
        writeln!("  add t0, fp, t0");
        match size {
            4 => writeln!("  fsw fa{}, 0(t0)", register),
            8 => writeln!("  fsd fa{}, 0(t0)", register),
            _ => {
                unreachable!();
            }
        }
    }

    /// 与0进行比较，不等于0则置1
    fn not_zero(&mut self, typ: TypeLink) {
        match typ.borrow().kind {
            TypeKind::Float => {
                writeln!("  # 判断fa1是否不为0，为0置0，非0置1");
                writeln!("  fmv.s.x fa1, zero");
                writeln!("  feq.s a0, fa0, fa1");
                writeln!("  xori a0, a0, 1");
            }
            TypeKind::Double => {
                writeln!("  # 判断fa1是否不为0，为0置0，非0置1");
                writeln!("  fmv.d.x fa1, zero");
                writeln!("  feq.d a0, fa0, fa1");
                writeln!("  xori a0, a0, 1");
            }
            _ => {}
        }
    }

    /// 类型转换
    fn cast(&mut self, from: TypeLink, to: TypeLink) {
        if to.borrow().kind == TypeKind::Void {
            return;
        }

        if to.borrow().kind == TypeKind::Bool {
            self.not_zero(from);
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
        self.counter += 1;
        let c = self.counter;
        c
    }
}

// 类型映射表
// 先逻辑左移N位，再算术右移N位，就实现了将64位有符号数转换为64-N位的有符号数
/// i64 -> i8
const I8I1: Option<&str> = Some("  # 转换为i8类型\n  slli a0, a0, 56\n  srai a0, a0, 56");
/// i64 -> i16
const I8I2: Option<&str> = Some("  # 转换为i16类型\n  slli a0, a0, 48\n  srai a0, a0, 48");
/// i64 -> i32
const I8I4: Option<&str> = Some("  # 转换为i32类型\n  slli a0, a0, 32\n  srai a0, a0, 32");
// 先逻辑左移N位，再逻辑右移N位，就实现了将64位无符号数转换为64-N位的无符号数
/// i64 -> u8
const I8U1: Option<&str> = Some("  # 转换为u8类型\n  slli a0, a0, 56\n  srli a0, a0, 56");
/// i64 -> u16
const I8U2: Option<&str> = Some("  # 转换为u16类型\n  slli a0, a0, 48\n  srli a0, a0, 48");
/// i64 -> u32
const I8U4: Option<&str> = Some("  # 转换为u32类型\n  slli a0, a0, 32\n  srli a0, a0, 32");
// 有符号整型转换为浮点数
/// i64 -> f32
const I8F4: Option<&str> = Some("  # i64转换为f32类型\n  fcvt.s.l fa0, a0");
/// i64 -> f64
const I8F8: Option<&str> = Some("  # i64转换为f64类型\n  fcvt.d.l fa0, a0");
// 无符号整型转换为浮点数
/// u64 -> f32
const U8F4: Option<&str> = Some("  # u64转换为f32类型\n  fcvt.s.lu fa0, a0");
/// u64 -> f64
const U8F8: Option<&str> = Some("  # u64转换为f64类型\n  fcvt.d.lu fa0, a0");
// 单精度浮点数转换为整型
/// f32 -> i8
const F4I1: Option<&str> =
    Some("  # f32转换为i8类型\n  fcvt.w.s a0, fa0, rtz\n  slli a0, a0, 56\n  srai a0, a0, 56\n");
/// f32 -> i16
const F4I2: Option<&str> =
    Some("  # f32转换为i16类型\n  fcvt.w.s a0, fa0, rtz\n  slli a0, a0, 48\n  srai a0, a0, 48\n");
/// f32 -> i32
const F4I4: Option<&str> = Some("  # f32转换为i32类型\n  fcvt.w.s a0, fa0, rtz");
/// f32 -> i64
const F4I8: Option<&str> = Some("  # f32转换为i64类型\n  fcvt.l.s a0, fa0, rtz");
// 无符号整型转换为无符号浮点数
/// f32 -> u8
const F4U1: Option<&str> =
    Some("  # f32转换为u8类型\n  fcvt.wu.s a0, fa0, rtz\n  slli a0, a0, 56\n  srli a0, a0, 56\n");
/// f32 -> u16
const F4U2: Option<&str> =
    Some("  # f32转换为u16类型\n  fcvt.wu.s a0, fa0, rtz\n  slli a0, a0, 48\n  srli a0, a0, 48\n");
/// f32 -> u32
const F4U4: Option<&str> = Some("  # f32转换为u32类型\n  fcvt.wu.s a0, fa0, rtz");
/// f32 -> u64
const F4U8: Option<&str> = Some("  # f32转换为u64类型\n  fcvt.lu.s a0, fa0, rtz");
// 单精度转换为双精度浮点数
/// f32 -> f64
const F4F8: Option<&str> = Some("  # f32转换为f64类型\n  fcvt.d.s fa0, fa0");
// 双精度浮点数转换为整型
/// f64 -> i8
const F8I1: Option<&str> =
    Some("  # f64转换为i8类型\n  fcvt.w.d a0, fa0, rtz\n  slli a0, a0, 56\n  srai a0, a0, 56\n");
/// f64 -> i16
const F8I2: Option<&str> =
    Some("  # f64转换为i16类型\n  fcvt.w.d a0, fa0, rtz\n  slli a0, a0, 48\n  srai a0, a0, 48\n");
/// f64 -> i32
const F8I4: Option<&str> = Some("  # f64转换为i32类型\n  fcvt.w.d a0, fa0, rtz");
/// f64 -> i64
const F8I8: Option<&str> = Some("  # f64转换为i64类型\n  fcvt.l.d a0, fa0, rtz");
// 双精度浮点数转换为无符号整型
/// f64 -> u8
const F8U1: Option<&str> =
    Some("  # f64转换为u8类型\n  fcvt.wu.d a0, fa0, rtz\n  slli a0, a0, 56\n  srli a0, a0, 56\n");
/// f64 -> u16
const F8U2: Option<&str> =
    Some("  # f64转换为u16类型\n  fcvt.wu.d a0, fa0, rtz\n  slli a0, a0, 48\n  srli a0, a0, 48\n");
/// f64 -> u32
const F8U4: Option<&str> = Some("  # f64转换为u32类型\n  fcvt.wu.d a0, fa0, rtz");
/// f64 -> u64
const F8U8: Option<&str> = Some("  # f64转换为u64类型\n  fcvt.lu.d a0, fa0, rtz");
// 双精度转换为单精度浮点数
/// f64 -> f32
const F8F4: Option<&str> = Some("  # f64转换为f32类型\n  fcvt.s.d fa0, fa0");

/// 获取类型对应的index
fn get_type_id(typ: TypeLink) -> usize {
    let result = match typ.borrow().kind {
        TypeKind::Char => 0,
        TypeKind::Short => 1,
        TypeKind::Int => 2,
        TypeKind::Float => 8,
        TypeKind::Double => 9,
        _ => 3,
    };
    if typ.borrow().is_unsigned {
        result + 4
    } else {
        result
    }
}

/// 所有类型转换表
const CAST_TABLE: [[Option<&str>; 10]; 10] = [
    //{i8, i16,  i32,  i64,  u8,   u16,  u32,  u64   f32   f64}
    [None, None, None, None, I8U1, None, None, None, I8F4, I8F8], // 从i8转换
    [I8I1, None, None, None, I8U1, I8U2, None, None, I8F4, I8F8], // 从i16转换
    [I8I1, I8I2, None, None, I8U1, I8U2, I8U4, None, I8F4, I8F8], // 从i32转换
    [I8I1, I8I2, I8I4, None, I8U1, I8U2, I8U4, None, I8F4, I8F8], // 从i64转换
    [I8I1, None, None, None, None, None, None, None, U8F4, U8F8], // 从u8转换
    [I8I1, I8I2, None, None, I8U1, None, None, None, U8F4, U8F8], // 从u16转换
    [I8I1, I8I2, I8I4, None, I8U1, I8U2, None, None, U8F4, U8F8], // 从u32转换
    [I8I1, I8I2, I8I4, None, I8U1, I8U2, I8U4, None, U8F4, U8F8], // 从u64转换
    [F4I1, F4I2, F4I4, F4I8, F4U1, F4U2, F4U4, F4U8, None, F4F8], // 从f32转换
    [F8I1, F8I2, F8I4, F8I8, F8U1, F8U2, F8U4, F8U8, F8F4, None], // 从f64转换
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
