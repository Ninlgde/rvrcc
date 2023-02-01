//! 汇编生成器

use crate::ctype::{cal_flo_st_mems_ty, TypeKind, TypeLink};
use crate::node::{NodeKind, NodeLink};
use crate::obj::{Obj, ObjLink};
use crate::{align_to, error_token, write_file, Args, FP_MAX, GP_MAX, INPUTS, OUTPUT};
use std::io::Write;
use std::{cmp, fmt};

/// 汇编代码生成到文件
pub fn codegen(program: &mut Vec<ObjLink>, args: &Args, write_file: &'static mut dyn Write) {
    unsafe {
        OUTPUT = Some(write_file);
    }
    let mut generator = Generator::new(program, args);
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
        $crate::codegen::write2output(format_args!(concat!($fmt, "") $(, $($arg)+)?))
    };
}

/// 生成器
struct Generator<'a> {
    /// ast
    program: &'a mut Vec<ObjLink>,
    /// args
    args: &'a Args,
    /// 当前生成的方法名
    current_function_name: String,
    /// 当前生成的方法
    current_function: Option<ObjLink>,
    /// 栈深
    depth: usize,
    /// 代码段计数
    counter: u32,
    /// 大结构体的深度
    bsdepth: usize,
}

impl<'a> Generator<'a> {
    pub fn new(program: &'a mut Vec<ObjLink>, args: &'a Args) -> Self {
        Generator {
            program,
            args,
            current_function_name: "".to_string(),
            current_function: None,
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
                    va_area,
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
                        let kind = typ.borrow().kind.clone();
                        match kind {
                            TypeKind::Struct | TypeKind::Union => {
                                // 判断结构体的类型
                                let tys = cal_flo_st_mems_ty(typ.clone(), gp, fp);
                                // 结构体的大小
                                let mut typ = typ.borrow_mut();
                                typ.fs_reg1ty = Some(tys[0].clone());
                                typ.fs_reg2ty = Some(tys[1].clone());
                                let ts = typ.size;
                                let fs_reg1ty = tys[0].clone();
                                let fs_reg2ty = tys[1].clone();
                                // 计算浮点结构体所使用的寄存器
                                // 这里一定寄存器可用，所以不判定是否超过寄存器最大值
                                if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
                                    for reg in tys.iter() {
                                        if reg.borrow().is_float() {
                                            fp += 1;
                                        }
                                        if reg.borrow().is_int() {
                                            gp += 1;
                                        }
                                    }
                                    continue;
                                }

                                // 9～16字节的结构体要用两个寄存器
                                if 8 < ts && ts <= 16 {
                                    // 如果只剩一个寄存器，那么剩余一半通过栈传递
                                    if gp == GP_MAX - 1 {
                                        var.set_is_half_by_stack(true);
                                    }
                                    if gp < GP_MAX {
                                        gp += 1;
                                    }
                                }
                                // 所有字节的结构体都在至少使用了一个寄存器（如果可用）
                                if gp < GP_MAX {
                                    gp += 1;
                                    continue;
                                }

                                // 没使用寄存器的需要栈传递
                            }
                            TypeKind::Float | TypeKind::Double => {
                                if fp < FP_MAX {
                                    writeln!(" #  FP{}传递浮点变量{}", fp, var.get_name());
                                    fp += 1;
                                    continue;
                                } else if gp < GP_MAX {
                                    writeln!(" #  GP{}传递浮点变量{}", gp, var.get_name());
                                    gp += 1;
                                    continue;
                                }
                            }
                            _ => {
                                if gp < GP_MAX {
                                    writeln!(" #  GP{}传递整型变量{}", gp, var.get_name());
                                    gp += 1;
                                    continue;
                                }
                            }
                        }
                        // 栈传递
                        // 对齐变量
                        re_offset = align_to(re_offset, 8);
                        // 为栈传递变量赋一个偏移量，或者说是反向栈地址
                        var.set_offset(re_offset);
                        // 栈传递变量计算反向偏移量，传递一半的结构体减去寄存器的部分
                        let is_half = var.get_is_half_by_stack();
                        let size = typ.borrow().size;
                        re_offset += if is_half { size - 8 } else { size };
                        writeln!(" #  栈传递变量{}偏移量{}", var.get_name(), var.get_offset());
                    }

                    if va_area.is_some() {
                        re_offset = align_to(re_offset, 8);
                        va_area.as_mut().unwrap().borrow_mut().set_offset(re_offset);
                    }

                    let mut offset = 0;
                    for var in locals.iter().rev() {
                        {
                            let cv = var.clone();
                            let v = cv.borrow();
                            if v.get_offset() != 0 && !v.get_is_half_by_stack() {
                                continue;
                            }
                            let t = v.get_type().borrow();
                            let align = if t.kind == TypeKind::Array && t.size > 16 {
                                cmp::max(16, v.get_align())
                            } else {
                                v.get_align()
                            };
                            offset += t.size as isize;
                            offset = align_to(offset, align);
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
                    is_tls,
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
                    // 数组超过16字节时，对齐值至少为16字节
                    let mut align = var.get_align();
                    if var.get_type().borrow().kind == TypeKind::Array
                        && var.get_type().borrow().size >= 16
                    {
                        align = cmp::max(16, var.get_align());
                    }
                    let ts = var.get_type().borrow().size;

                    // 为试探性的全局变量生成指示
                    if self.args.opt_f_common && var.is_tentative() {
                        writeln!("  .comm {}, {}, {}", var.get_name(), ts, align);
                        continue;
                    }

                    // 判断是否有初始值
                    // .data 或 .tdata 段
                    if init_data.is_some() {
                        if *is_tls {
                            writeln!("\n  # TLS数据段标签");
                            // a：可加载执行
                            // w：可写
                            // T：线程局部的
                            // progbits：包含程序数据
                            writeln!("  .section .tdata,\"awT\",@progbits");
                        } else {
                            writeln!("\n  # 数据段标签");
                            writeln!("  .data");
                        }

                        writeln!("  .type {}, @object", name);
                        writeln!("  .size {}, {}", name, ts);
                        writeln!("  .align {}", simple_log2(align));
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
                                    let i = chars[pos as usize];
                                    let c = i as u8 as char;
                                    if c.is_ascii() && !c.is_ascii_control() {
                                        writeln!("  .byte {}\t# 字符：{}", i, c);
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
                    // .bss 或 .tbss 段
                    if *is_tls {
                        // nobits：不含数据
                        writeln!("\n  # TLS未初始化的全局变量");
                        writeln!("  .section .tbss,\"awT\",@nobits");
                    } else {
                        writeln!("  # 未初始化的全局变量");
                        writeln!("  .bss");
                    }
                    writeln!("  .align {}", simple_log2(align));
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
        for func in self.program.to_vec().iter() {
            let function = &*func.borrow();
            match function {
                Obj::Func {
                    name,
                    body,
                    params,
                    stack_size,
                    is_definition,
                    is_static,
                    is_live,
                    va_area,
                    alloca_bottom,
                    ..
                } => {
                    if !is_definition {
                        continue;
                    }

                    // No code is emitted for "static inline" functions
                    // if no one is referencing them.
                    if !is_live {
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
                    writeln!("  .type {}, @function", name);
                    writeln!("{}:", name);
                    self.current_function_name = name.to_string();
                    self.current_function = Some(func.clone());

                    // 栈布局
                    // ------------------------------//
                    //        上一级函数的栈传递参数
                    // ==============================// sp（本级函数）
                    //         VaArea(寄存器可用时)
                    // ------------------------------// sp = sp（本级函数）-VaArea
                    //              ra
                    //-------------------------------// ra = sp-8
                    //              fp
                    //-------------------------------// fp = sp-16
                    //             变量
                    //-------------------------------// sp = sp-16-StackSize
                    //           表达式计算
                    //-------------------------------//

                    // Prologue, 前言

                    // 为剩余的整型寄存器开辟空间，用于存储可变参数
                    let mut va_size = 0;
                    if va_area.is_some() {
                        // 遍历正常参数所使用的浮点、整型寄存器
                        let mut gps = 0;
                        let mut fps = 0;
                        // 可变参数函数，非可变的参数使用寄存器
                        for param in params.iter().rev() {
                            let pt = param.borrow().get_type().clone();
                            if pt.borrow().is_float() && fps < FP_MAX {
                                // 可变参数函数中的浮点参数
                                fps += 1;
                            } else if gps < GP_MAX {
                                // 可变参数函数中的整型参数
                                gps += 1;
                            }
                        }
                        va_size = (8 - gps) * 8;
                        writeln!("  # VaArea的区域，大小为{}", va_size);
                        writeln!("  addi sp, sp, -{}", va_size);
                    }
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
                    // Alloca函数
                    writeln!("  # 将当前的sp值，存入到Alloca区域的底部");
                    let offset = alloca_bottom.as_ref().unwrap().borrow().get_offset();
                    writeln!("  li t0, {}", offset);
                    writeln!("  add t0, t0, fp");
                    writeln!("  sd sp, 0(t0)");

                    // 正常传递的形参
                    // 记录整型寄存器，浮点寄存器使用的数量
                    let mut gp = 0;
                    let mut fp = 0;
                    for param in params.iter().rev() {
                        let var = param.borrow();
                        let typ = var.get_type().clone();
                        if var.get_offset() > 0 && !var.get_is_half_by_stack() {
                            continue;
                        }
                        let typ = typ.borrow();
                        let ts = typ.size;
                        let offset = var.get_offset();
                        // 正常传递的形参
                        match typ.kind {
                            TypeKind::Struct | TypeKind::Union => {
                                writeln!("  # 对寄存器传递的结构体进行压栈");
                                let fs_reg1ty = typ.fs_reg1ty.as_ref().unwrap();
                                let fs_reg2ty = typ.fs_reg2ty.as_ref().unwrap();
                                // 对寄存器传递的参数进行压栈
                                if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
                                    writeln!("  # 浮点结构体的第一部分进行压栈");
                                    // 浮点寄存器的第一部分
                                    let size1 = fs_reg1ty.borrow().size;
                                    if fs_reg1ty.borrow().is_float() {
                                        self.store_float(fp, offset, size1);
                                        fp += 1;
                                    }
                                    if fs_reg1ty.borrow().is_int() {
                                        self.store_general(gp, offset, cmp::min(8, ts));
                                        gp += 1;
                                    }

                                    // 浮点寄存器的第二部分
                                    if fs_reg2ty.borrow().kind != TypeKind::Void {
                                        writeln!("  # 浮点结构体的第二部分进行压栈");
                                        let size2 = fs_reg2ty.borrow().size;
                                        let off2 = cmp::max(size1, size2);
                                        if fs_reg2ty.borrow().is_float() {
                                            self.store_float(fp, offset + off2, size2);
                                            fp += 1;
                                        }
                                        if fs_reg2ty.borrow().is_int() {
                                            self.store_general(gp, offset + off2, size2);
                                            gp += 1;
                                        }
                                    }
                                } else {
                                    // 大于16字节的结构体参数，通过访问它的地址，
                                    // 将原来位置的结构体复制到栈中
                                    if ts > 16 {
                                        self.store_struct(gp, offset, ts);
                                        gp += 1;
                                    } else {
                                        // 一半寄存器，一半栈传递的结构体
                                        if var.get_is_half_by_stack() {
                                            self.store_general(gp, offset, 8);
                                            gp += 1;
                                            // 拷贝栈传递的一半结构体到当前栈中
                                            for i in 0..ts - 8 {
                                                writeln!("  lb t0, {}(fp)", 16 + i);
                                                writeln!("  li t1, {}", offset + 8 + i);
                                                writeln!("  add t1, fp, t1");
                                                writeln!("  sb t0, 0(t1)");
                                            }
                                        } else {
                                            // 处理小于16字节的结构体
                                            if ts <= 16 {
                                                self.store_general(gp, offset, cmp::min(8, ts));
                                                gp += 1;
                                            }
                                            if ts > 8 {
                                                self.store_general(gp, offset + 8, ts - 8);
                                                gp += 1;
                                            }
                                        }
                                    }
                                }
                            }
                            TypeKind::Float | TypeKind::Double => {
                                if fp < FP_MAX {
                                    writeln!(
                                        "  # 将浮点形参{}的寄存器fa{}的值压栈",
                                        var.get_name(),
                                        fp
                                    );
                                    self.store_float(fp, offset, ts);
                                    fp += 1;
                                } else {
                                    writeln!(
                                        "  # 将浮点形参{}的寄存器a{}的值压栈",
                                        var.get_name(),
                                        gp
                                    );
                                    self.store_general(gp, offset, ts);
                                    gp += 1;
                                }
                            }
                            _ => {
                                writeln!("  # 将整型形参{}的寄存器a{}的值压栈", var.get_name(), gp);
                                self.store_general(gp, offset, ts);
                                gp += 1;
                            }
                        }
                    }

                    // 可变参数
                    if va_area.is_some() {
                        // 可变参数位置位于本函数的最上方，即sp的位置，也就是fp+16
                        // 可变参数存入__va_area__，注意最多为7个
                        let va_area = va_area.as_ref().unwrap().borrow();
                        let mut offset = va_area.get_offset();
                        writeln!("  # 可变参数VaArea的偏移量为{}", offset);
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

                    // main默认返回0
                    if name.eq("main") {
                        writeln!("  li a0, 0");
                    }

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
                    // 归还可变参数寄存器压栈的那一部分
                    if va_area.is_some() {
                        writeln!("  # 归还VaArea的区域，大小为{}", va_size);
                        writeln!("  addi sp, sp, {}", va_size);
                    }
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
                    let lhs = node.lhs.as_ref().unwrap();
                    self.gen_expr(lhs);

                    // 处理结构体作为返回值的情况
                    let typ = lhs.typ.as_ref().unwrap().clone();
                    if typ.borrow().is_struct_union() {
                        if typ.borrow().size <= 16 {
                            // 小于16字节拷贝寄存器
                            self.copy_struct_reg();
                        } else {
                            // 大于16字节拷贝内存
                            self.copy_struct_mem();
                        }
                    }
                }
                // 无条件跳转语句，跳转到.L.return段
                // j offset是 jal x0, offset的别名指令
                writeln!("  # 跳转到.L.return.{}段", self.current_function_name);
                writeln!("  j .L.return.{}", self.current_function_name);
            }
            NodeKind::Asm => {
                writeln!("  {}", node.asm_str);
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

                let typ = node.typ.as_ref().unwrap();
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
                        if typ.size <= 4 {
                            writeln!("  negw a0, a0");
                        } else {
                            writeln!("  neg a0, a0");
                        }
                    }
                }
                return;
            }
            // 变量
            NodeKind::Var => {
                // 计算出变量的地址，然后存入a0
                self.gen_addr(node);
                self.load(node.typ.as_ref().unwrap().clone());
                return;
            }
            // 成员变量
            NodeKind::Member => {
                // 计算出变量的地址，然后存入a0
                self.gen_addr(node);
                self.load(node.typ.as_ref().unwrap().clone());

                let member = node.member.as_ref().unwrap();
                if member.is_bitfield {
                    writeln!(
                        "  # 清除位域的成员变量（{}字节）未用到的位",
                        member.bit_width
                    );
                    // 清除位域成员变量未用到的高位
                    writeln!(
                        "  slli a0, a0, {}",
                        64 - member.bit_width - member.bit_offset
                    );
                    // 清除位域成员变量未用到的低位
                    if member.typ.as_ref().unwrap().borrow().is_unsigned {
                        writeln!("  srli a0, a0, {}", 64 - member.bit_width);
                    } else {
                        writeln!("  srai a0, a0, {}", 64 - member.bit_width);
                    }
                }
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
                let lhs = node.lhs.as_ref().unwrap();
                self.gen_addr(lhs);
                self.push();
                // 右部是右值，为表达式的值
                self.gen_expr(node.rhs.as_ref().unwrap());

                // 如果是位域成员变量，需要先从内存中读取当前值，然后合并到新值中
                if lhs.kind == NodeKind::Member && lhs.member.as_ref().unwrap().is_bitfield {
                    writeln!("\n  # 位域成员变量进行赋值↓");
                    writeln!("  # 备份需要赋的a0值");
                    writeln!("  mv t2, a0");
                    writeln!("  # 计算位域成员变量的新值：");
                    let member = lhs.member.as_ref().unwrap();
                    // 将需要赋的值a0存入t1
                    writeln!("  mv t1, a0");
                    // 构造一个和位域成员长度相同，全为1的二进制数
                    writeln!("  li t0, {}", (1i64 << member.bit_width) - 1);
                    // 取交之后，位域长度的低位，存储了我们需要的值，其他位都为0
                    writeln!("  and t1, t1, t0");
                    // 然后将该值左移，相应的位偏移量中
                    // 此时我们所需要的位域数值已经处于正确的位置，且其他位置都为0
                    writeln!("  slli t1, t1, {}", member.bit_offset);

                    writeln!("  # 读取位域当前值：");
                    // 将位域值保存的地址加载进来
                    writeln!("  ld a0, 0(sp)");
                    // 读取该地址的值
                    self.load(member.typ.as_ref().unwrap().clone());

                    writeln!("  # 写入成员变量新值到位域当前值中：");
                    // 位域值对应的掩码，即t1需要写入的位置
                    // 掩码位都为1，其余位为0
                    let mask = ((1i64 << member.bit_width) - 1) << member.bit_offset;
                    // 对掩码取反，此时，其余位都为1，掩码位都为0
                    writeln!("  li t0, {}", !mask);
                    // 取交，保留除掩码位外所有的位
                    writeln!("  and a0, a0, t0");
                    // 取或，将成员变量的新值写入到掩码位
                    writeln!("  or a0, a0, t1");
                    self.store(node.typ.as_ref().unwrap().clone());
                    writeln!("  # 恢复需要赋的a0值作为返回值");
                    writeln!("  mv a0, t2");
                    writeln!("  # 完成位域成员变量的赋值↑\n");
                    return;
                }

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
                // 对alloca函数进行处理
                let lhs = node.lhs.as_ref().unwrap();
                if lhs.kind == NodeKind::Var {
                    let var = lhs.var.as_ref().unwrap();
                    if var.borrow().get_name().eq("alloca") {
                        // 解析alloca函数的参数，确定开辟空间的字节数
                        self.gen_expr(node.args.first().as_ref().unwrap());
                        // 将需要的字节数存入t1
                        writeln!("  mv t1, a0");
                        // 生成Alloca函数汇编
                        self.builtin_alloca();
                        return;
                    }
                }
                // 计算所有参数的值，正向压栈
                // 此处获取到栈传递参数的数量
                let stack = self.push_args(&node);
                self.gen_expr(node.lhs.as_ref().unwrap());
                // 将a0的值存入t5
                writeln!("  mv t5, a0");

                // 反向弹栈，a0->参数1，a1->参数2……
                let mut gp = 0;
                let mut fp = 0;
                let mut pi = 0;

                if node.ret_buf.is_some() && node.typ.as_ref().unwrap().borrow().size > 16 {
                    writeln!("  # 返回结构体大于16字节，那么第一个参数指向返回缓冲区");
                    self.pop(gp);
                    gp += 1;
                }

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
                writeln!("  jalr t5");

                // 回收为栈传递的变量开辟的栈空间
                if stack > 0 {
                    // 栈的深度减去栈传递参数的字节数
                    self.depth -= stack;
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

                // 如果返回的结构体小于16字节，直接使用寄存器返回
                if node.ret_buf.is_some() && node.typ.as_ref().unwrap().borrow().size <= 16 {
                    let ret_buf = node.ret_buf.as_ref().unwrap();
                    self.copy_ret_buffer(ret_buf);
                    writeln!("  li t0, {}", ret_buf.borrow().get_offset());
                    writeln!("  add a0, fp, t0");
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

    /// 处理浮点类型
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
            // VLA可变长度数组是局部变量
            if var.get_type().borrow().kind == TypeKind::VLA {
                writeln!("  # 为VLA生成局部变量");
                writeln!("  li t0, {}", var.get_offset());
                writeln!("  add t0, t0, fp");
                writeln!("  ld a0, 0(t0)");
                return;
            }
            // 局部变量
            if var.is_local() {
                // 偏移量是相对于fp的
                writeln!(
                    "  # 获取局部变量{}的栈内地址为{}(fp)",
                    var.get_name(),
                    var.get_offset()
                );
                writeln!("  li t0, {}", var.get_offset());
                writeln!("  add a0, fp, t0");
                return;
            }

            if var.is_tls() {
                // 计算TLS高20位地址
                writeln!("  lui a0, %tprel_hi({})", var.get_name());
                // 计算TLS低12位地址
                writeln!("  addi a0, a0, %tprel_lo({})", var.get_name());
                return;
            }

            let typ = node.typ.as_ref().unwrap();
            // 函数
            if typ.borrow().kind == TypeKind::Func {
                if var.is_definition() {
                    // 定义的函数
                    writeln!("  # 获取函数{}的地址", var.get_name());
                    writeln!("  la a0, {}", var.get_name());
                } else {
                    // 外部函数
                    let c = self.count();
                    writeln!("  # 获取外部函数的绝对地址");
                    writeln!(".Lpcrel_hi{}:", c);
                    // 高20位地址，存到a0中
                    writeln!("  auipc a0, %got_pcrel_hi({})", var.get_name());
                    // 低12位地址，加到a0中
                    writeln!("  ld a0, %pcrel_lo(.Lpcrel_hi{})(a0)", c);
                }
                return;
            }

            // 全局变量
            let c = self.count();
            writeln!("  # 获取全局变量的绝对地址");
            writeln!(".Lpcrel_hi{}:", c);
            // 高20位地址，存到a0中
            writeln!("  auipc a0, %got_pcrel_hi({})", var.get_name());
            // 低12位地址，加到a0中
            writeln!("  ld a0, %pcrel_lo(.Lpcrel_hi{})(a0)", c);
            return;
        } else if node.kind == NodeKind::DeRef {
            // 解引用*
            self.gen_expr(node.lhs.as_ref().unwrap());
            return;
        } else if node.kind == NodeKind::Comma {
            // 逗号
            self.gen_expr(node.lhs.as_ref().unwrap());
            self.gen_addr(node.rhs.as_ref().unwrap());
            return;
        } else if node.kind == NodeKind::Member {
            // 逗号
            self.gen_addr(node.lhs.as_ref().unwrap());
            writeln!("  # 计算成员变量的地址偏移量");
            let offset = node.member.as_ref().unwrap().offset;
            writeln!("  li t0, {}", offset);
            writeln!("  add a0, a0, t0");
            return;
        } else if node.kind == NodeKind::FuncCall {
            // 如果存在返回值缓冲区
            if node.ret_buf.is_some() {
                self.gen_expr(node);
                return;
            }
        } else if node.kind == NodeKind::VLAPtr {
            // VLA的指针
            let var = &node.var;
            let var = &*var.as_ref().unwrap().borrow();
            writeln!("  # 生成VLA的指针");
            writeln!("  li t0, {}", var.get_offset());
            writeln!("  add a0, t0, fp");
            return;
        }
        error_token!(&node.get_token(), "not an lvalue")
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
    fn push_args(&mut self, node: &NodeLink) -> usize {
        let mut stack = 0;
        let mut fp = 0;
        let mut gp = 0;

        // 如果是超过16字节的结构体，则通过第一个寄存器传递结构体的指针
        if node.ret_buf.is_some() && node.typ.as_ref().unwrap().borrow().size > 16 {
            gp += 1;
        }

        let nft = node.func_type.as_ref().unwrap();
        let binding = nft.borrow();
        let mut params_peek = binding.params.iter().peekable();

        // 遍历所有参数，优先使用寄存器传递，然后是栈传递
        let mut args = node.args.to_vec();
        for arg in args.iter_mut() {
            // 如果是可变参数的参数，只使用整型寄存器和栈传递
            if binding.is_variadic && params_peek.peek().is_none() {
                let val = if arg.val == 0 {
                    arg.fval.to_bits() as i64
                } else {
                    arg.val
                };
                if gp < GP_MAX {
                    writeln!("  # 可变参数{}值通过a{}传递", val, gp);
                    gp += 1;
                } else {
                    writeln!("  # 可变参数{}值通过栈传递", val);
                    arg.pass_by_stack = true;
                    stack += 1;
                }
                continue;
            }
            // 遍历相应的实参，用于检查是不是到了可变参数
            params_peek.next();

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
            stack += 1;
        }

        // 进行压栈
        // 开辟大于16字节的结构体的栈空间
        let bsstack = self.create_bsspace(&args);
        // 第一遍对栈传递的变量进行压栈
        self.push_args0(&args, true);
        // 第二遍对寄存器传递的变量进行压栈
        self.push_args0(&args, false);

        if node.ret_buf.is_some() && node.typ.as_ref().unwrap().borrow().size > 16 {
            let ret = node.ret_buf.as_ref().unwrap();
            writeln!("  # 返回类型是大于16字节的结构体，指向其的指针，压入栈顶");
            writeln!("  li t0, {}", ret.borrow().get_offset());
            writeln!("  add a0, fp, t0");
            self.push();
        }
        // 返回栈传递参数的个数
        stack + bsstack
    }

    /// 复制结构体返回值到缓冲区中
    fn copy_ret_buffer(&mut self, var: &ObjLink) {
        let binding = var.borrow();
        let typ = binding.get_type();
        let mut gp = 0;
        let mut fp = 0;

        let tys = cal_flo_st_mems_ty(typ.clone(), gp, fp);
        let mut typ = typ.borrow_mut();
        typ.fs_reg1ty = Some(tys[0].clone());
        typ.fs_reg2ty = Some(tys[1].clone());
        let fs_reg1ty = tys[0].clone();
        let fs_reg2ty = tys[1].clone();

        writeln!("  # 拷贝到返回缓冲区");
        writeln!("  # 加载struct地址到t0");
        writeln!("  li t0, {}", var.borrow().get_offset());
        writeln!("  add t1, fp, t0");

        // 处理浮点结构体的情况
        if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
            let mut offset = 0;
            for reg in tys.iter() {
                let kind = reg.borrow().kind.clone();
                match kind {
                    TypeKind::Float => {
                        writeln!("  fsw fa{}, {}(t1)", fp, offset);
                        offset = 4;
                        fp += 1;
                    }
                    TypeKind::Double => {
                        writeln!("  fsd fa{}, {}(t1)", fp, offset);
                        offset = 8;
                        fp += 1;
                    }
                    TypeKind::Void => {}
                    _ => {
                        writeln!("  sd a{}, {}(t1)", gp, offset);
                        offset = 8;
                        gp += 1;
                    }
                }
            }
            return;
        }

        writeln!("  # 复制整型结构体返回值到缓冲区中");
        let mut offset = 0;
        while offset < typ.size {
            match typ.size - offset {
                1 => {
                    writeln!("  sb a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                2 => {
                    writeln!("  sh a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                3 | 4 => {
                    writeln!("  sw a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                _ => {
                    writeln!("  sd a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
            }
            offset += 8;
        }
    }

    /// 拷贝结构体的寄存器
    fn copy_struct_reg(&mut self) {
        let cf = self.current_function.as_ref().unwrap().borrow();
        let cft = cf.get_type().clone();
        let crt = cft.borrow().return_type.as_ref().unwrap().clone();
        let mut gp = 0;
        let mut fp = 0;

        writeln!("  # 复制结构体寄存器");
        writeln!("  # 读取寄存器，写入存有struct地址的0(t1)中");
        writeln!("  mv t1, a0");

        let tys = cal_flo_st_mems_ty(crt.clone(), gp, fp);
        let mut typ = crt.borrow_mut();
        typ.fs_reg1ty = Some(tys[0].clone());
        typ.fs_reg2ty = Some(tys[1].clone());
        let fs_reg1ty = tys[0].clone();
        let fs_reg2ty = tys[1].clone();

        if fs_reg1ty.borrow().is_float() || fs_reg2ty.borrow().is_float() {
            let mut offset = 0;
            for reg in tys.iter() {
                let kind = reg.borrow().kind.clone();
                match kind {
                    TypeKind::Float => {
                        writeln!("  flw fa{}, {}(t1)", fp, offset);
                        offset = 4;
                        fp += 1;
                    }
                    TypeKind::Double => {
                        writeln!("  fld fa{}, {}(t1)", fp, offset);
                        offset = 8;
                        fp += 1;
                    }
                    TypeKind::Void => {}
                    _ => {
                        writeln!("  ld a{}, {}(t1)", gp, offset);
                        offset = 8;
                        gp += 1;
                    }
                }
            }
            return;
        }

        writeln!("  # 复制返回的整型结构体的值");
        let mut offset = 0;
        while offset < typ.size {
            match typ.size - offset {
                1 => {
                    writeln!("  lb a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                2 => {
                    writeln!("  lh a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                3 | 4 => {
                    writeln!("  lw a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
                _ => {
                    writeln!("  ld a{}, {}(t1)", gp, offset);
                    gp += 1;
                }
            }
            offset += 8;
        }
    }

    /// 大于16字节的结构体返回值，需要拷贝内存
    fn copy_struct_mem(&mut self) {
        let cf = self.current_function.as_ref().unwrap().borrow();
        let cft = cf.get_type().clone();
        let crt = cft.borrow().return_type.as_ref().unwrap().clone();
        let var = cf.get_params().first().unwrap();

        writeln!("  # 复制大于16字节结构体内存");
        writeln!("  # 将栈内struct地址存入t1，调用者的结构体的地址");
        writeln!("  li t0, {}", var.borrow().get_offset());
        writeln!("  add t0, fp, t0");
        writeln!("  ld t1, 0(t0)");

        writeln!("  # 遍历结构体并从a0位置复制所有字节到t1");
        for i in 0..crt.borrow().size {
            writeln!("  lb t0, {}(a0)", i);
            writeln!("  sb t0, {}(t1)", i);
        }
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
    }

    /// 弹栈，将sp指向的地址的值，弹出到a1
    fn pop(&mut self, reg: usize) {
        writeln!("  # 弹栈，将栈顶的值存入a{}", reg);
        writeln!("  ld a{}, 0(sp)", reg);
        writeln!("  addi sp, sp, 8");
        self.depth -= 1;
    }

    /// 对于浮点类型进行压栈
    fn push_float(&mut self) {
        writeln!("  # 压栈，将fa0的值存入栈顶");
        writeln!("  addi sp, sp, -8");
        writeln!("  fsd fa0, 0(sp)");
        self.depth += 1;
    }

    /// 对于浮点类型进行弹栈
    fn pop_float(&mut self, reg: usize) {
        writeln!("  # 弹栈，将栈顶的值存入fa{}", reg);
        writeln!("  fld fa{}, 0(sp)", reg);
        writeln!("  addi sp, sp, 8");
        self.depth -= 1;
    }

    /// 加载a0指向的值
    fn load(&mut self, typ: TypeLink) {
        if typ.borrow().kind == TypeKind::Array
            || typ.borrow().kind == TypeKind::Struct
            || typ.borrow().kind == TypeKind::Union
            || typ.borrow().kind == TypeKind::Func
            || typ.borrow().kind == TypeKind::VLA
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

    /// 存储结构体到栈内开辟的空间
    fn store_struct(&mut self, register: usize, offset: isize, size: isize) {
        // t0是结构体的地址，复制t0指向的结构体到栈相应的位置中
        for i in 0..size {
            writeln!("  lb t0, {}(a{})", i, register);
            writeln!("  li t1, {}", offset + i);
            writeln!("  add t1, fp, t1");
            writeln!("  sb t0, 0(t1)");
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

    /// 开辟Alloca空间
    fn builtin_alloca(&mut self) {
        let alloca_bottom = self
            .current_function
            .as_ref()
            .unwrap()
            .borrow()
            .get_alloca_bottom();
        let offset = alloca_bottom.as_ref().unwrap().borrow().get_offset();
        // 对齐需要的空间t1到16字节
        //
        // 加上15，然后去除最低位的十六进制数
        writeln!("  addi t1, t1, 15");
        writeln!("  andi t1, t1, -16");

        // 注意t2与t1大小不定，仅为示例
        // ----------------------------- 旧sp（AllocaBottom所存的sp）
        // - - - - - - - - - - - - - - -
        //  需要在此开辟大小为t1的Alloca区域
        // - - - - - - - - - - - - - - -
        //            ↑
        //    t2（旧sp和新sp间的距离）
        //            ↓
        // ----------------------------- 新sp ← sp

        // 根据t1的值，提升临时区域
        //
        // 加载 旧sp 到t2中
        writeln!("  li t0, {}", offset);
        writeln!("  add t0, fp, t0");
        writeln!("  ld t2, 0(t0)");
        // t2=旧sp-新sp，将二者的距离存入t2
        writeln!("  sub t2, t2, sp");

        // 保存 新sp 存入a0
        writeln!("  mv a0, sp");
        // 新sp 开辟（减去）所需要的空间数，结果存入 sp
        // 并将 新sp开辟空间后的栈顶 同时存入t3
        writeln!("  sub sp, sp, t1");
        writeln!("  mv t3, sp");

        // 注意t2与t1大小不定，仅为示例
        // ----------------------------- 旧sp（AllocaBottom所存的sp）
        //              ↑
        //      t2（旧sp和新sp间的距离）
        //              ↓
        // ----------------------------- 新sp  ← a0
        //              ↑
        //     t1（Alloca所需要的空间数）
        //              ↓
        // ----------------------------- 新新sp ← sp,t3

        // 将 新sp内（底部和顶部间的）数据，复制到 新sp的顶部之上
        writeln!("1:");
        // t2为0时跳转到标签2，结束复制
        writeln!("beqz t2, 2f");
        // 将 新sp底部 内容复制到 新sp顶部之上
        writeln!("  lb t0, 0(a0)");
        writeln!("  sb t0, 0(t3)");
        writeln!("  addi a0, a0, 1");
        writeln!("  addi t3, t3, 1");
        writeln!("  addi t2, t2, -1");
        writeln!("  j 1b");
        writeln!("2:");

        // 注意t2与t1大小不定，仅为示例
        // ------------------------------ 旧sp   a0
        //             ↑                         ↓
        //       t1（Alloca区域）
        //             ↓
        // ------------------------------ 新sp ← a0
        //             ↑
        //  t2（旧sp和新sp间的内容，复制到此）
        //             ↓
        // ------------------------------ 新新sp ← sp

        // 移动alloca_bottom指针
        //
        // 加载 旧sp 到 a0
        writeln!("  li t0, {}", offset);
        writeln!("  add t0, fp, t0");
        writeln!("  ld a0, 0(t0)");
        // 旧sp 减去开辟的空间 t1
        writeln!("  sub a0, a0, t1");
        // 存储a0到alloca底部地址
        writeln!("sd a0, 0(t0)");
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
const F4I4: Option<&str> =
    Some("  # f32转换为i32类型\n  fcvt.w.s a0, fa0, rtz\n  slli a0, a0, 32\n  srai a0, a0, 32");
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
