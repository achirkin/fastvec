; ModuleID = 'prims/FloatX3Ops.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @ltFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 1) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = or i32 %4, 1
  %6 = sext i32 %5 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @gtFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %rhs, <4 x float> %lhs, i8 1) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = or i32 %4, 1
  %6 = sext i32 %5 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @leFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 2) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = or i32 %4, 1
  %6 = sext i32 %5 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @geFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %rhs, <4 x float> %lhs, i8 2) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = or i32 %4, 1
  %6 = sext i32 %5 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @eqFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 0) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = or i32 %4, 1
  %6 = sext i32 %5 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @maxFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.max.ps(<4 x float> %lhs, <4 x float> %rhs) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @minFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.min.ps(<4 x float> %lhs, <4 x float> %rhs) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @absFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = bitcast <4 x float> %argvec to <4 x i32>
  %4 = and <4 x i32> %3, <i32 2147483647, i32 2147483647, i32 2147483647, i32 2147483647>
  %5 = bitcast <4 x i32> %4 to <4 x float>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %5) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @signumFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = bitcast <4 x float> %argvec to <4 x i32>
  %4 = and <4 x i32> %3, <i32 -2147483648, i32 -2147483648, i32 -2147483648, i32 -2147483648>
  %5 = or <4 x i32> %4, <i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216>
  %6 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %argvec, <4 x float> zeroinitializer, i8 4) #2
  %7 = bitcast <4 x float> %6 to <4 x i32>
  %8 = and <4 x i32> %5, %7
  %9 = bitcast <4 x i32> %8 to <4 x float>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %9) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @recipFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.rcp.ps(<4 x float> %argvec) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @sqrtFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.sqrt.ps(<4 x float> %argvec) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @inverseMFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %c1, <4 x float> %c2, <4 x float> %c3) #0 {
  %1 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 1, i32 0, i32 6, i32 5>
  %2 = shufflevector <4 x float> %1, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %3 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 2, i32 1, i32 6, i32 4>
  %4 = fmul <4 x float> %2, %3
  %5 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 2, i32 0, i32 5, i32 6>
  %6 = shufflevector <4 x float> %5, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %7 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 1, i32 2, i32 5, i32 4>
  %8 = fmul <4 x float> %6, %7
  %9 = fsub <4 x float> %4, %8
  %10 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 0, i32 0, i32 6, i32 4>
  %11 = shufflevector <4 x float> %10, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %12 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 2, i32 0, i32 6, i32 4>
  %13 = fmul <4 x float> %11, %12
  %14 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 2, i32 0, i32 4, i32 6>
  %15 = shufflevector <4 x float> %14, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %16 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 0, i32 2, i32 4, i32 4>
  %17 = fmul <4 x float> %15, %16
  %18 = fsub <4 x float> %17, %13
  %19 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 0, i32 0, i32 5, i32 4>
  %20 = shufflevector <4 x float> %19, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %21 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 1, i32 0, i32 5, i32 4>
  %22 = fmul <4 x float> %20, %21
  %23 = shufflevector <4 x float> %c2, <4 x float> %c1, <4 x i32> <i32 1, i32 0, i32 4, i32 5>
  %24 = shufflevector <4 x float> %23, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 3, i32 1>
  %25 = shufflevector <4 x float> %c3, <4 x float> %c2, <4 x i32> <i32 0, i32 1, i32 4, i32 4>
  %26 = fmul <4 x float> %24, %25
  %27 = fsub <4 x float> %22, %26
  %28 = shufflevector <4 x float> %c1, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 undef, i32 undef>
  %29 = shufflevector <4 x float> %28, <4 x float> %c3, <4 x i32> <i32 0, i32 1, i32 6, i32 7>
  %30 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %29, <4 x float> %9, i32 127)
  %31 = fdiv <4 x float> <float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00>, %30
  %32 = load i64* %sp, align 8, !tbaa !1
  %33 = inttoptr i64 %32 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>)*
  %34 = fmul <4 x float> %9, %31
  %35 = fmul <4 x float> %18, %31
  %36 = fmul <4 x float> %27, %31
  tail call cc 10 void %33(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %34, <4 x float> %35, <4 x float> %36) #3
  ret void
}

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse41.dpps(<4 x float>, <4 x float>, i32) #1

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMMFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in10, <4 x float> %in11, <4 x float> %in12, <4 x float> %in20, <4 x float> %in21, <4 x float> %in22) #0 align 8 {
  %1 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> zeroinitializer
  %2 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %3 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %4 = fmul <4 x float> %1, %in10
  %5 = fmul <4 x float> %2, %in11
  %6 = fmul <4 x float> %3, %in12
  %7 = fadd <4 x float> %4, %5
  %8 = fadd <4 x float> %6, %7
  %9 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> zeroinitializer
  %10 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %11 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %12 = fmul <4 x float> %9, %in10
  %13 = fmul <4 x float> %10, %in11
  %14 = fmul <4 x float> %11, %in12
  %15 = fadd <4 x float> %12, %13
  %16 = fadd <4 x float> %14, %15
  %17 = shufflevector <4 x float> %in22, <4 x float> undef, <4 x i32> zeroinitializer
  %18 = shufflevector <4 x float> %in22, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %19 = shufflevector <4 x float> %in22, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %20 = fmul <4 x float> %17, %in10
  %21 = fmul <4 x float> %18, %in11
  %22 = fmul <4 x float> %19, %in12
  %23 = fadd <4 x float> %20, %21
  %24 = fadd <4 x float> %22, %23
  %25 = load i64* %sp, align 8, !tbaa !1
  %26 = inttoptr i64 %25 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>)*
  tail call cc 10 void %26(i64* %baseReg, i64* %sp, i64* %hp, i64 %r1, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %8, <4 x float> %16, <4 x float> %24) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMVFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %m1, <4 x float> %m2, <4 x float> %m3, <4 x float> %vec) #0 align 8 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> zeroinitializer
  %4 = fmul <4 x float> %3, %m1
  %5 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %6 = fmul <4 x float> %5, %m2
  %7 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %8 = fmul <4 x float> %7, %m3
  %9 = fadd <4 x float> %6, %8
  %10 = fadd <4 x float> %4, %9
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %10) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @transposeMFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in0, <4 x float> %in1, <4 x float> %in2) #0 {
  %1 = shufflevector <4 x float> %in0, <4 x float> %in1, <4 x i32> <i32 0, i32 1, i32 4, i32 5>
  %2 = shufflevector <4 x float> %in0, <4 x float> %in1, <4 x i32> <i32 2, i32 undef, i32 6, i32 undef>
  %3 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 0, i32 1, i32 0, i32 1>
  %4 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 2, i32 undef, i32 2, i32 undef>
  %5 = load i64* %sp, align 8, !tbaa !1
  %6 = inttoptr i64 %5 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>)*
  %7 = shufflevector <4 x float> %1, <4 x float> %3, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %8 = shufflevector <4 x float> %1, <4 x float> %3, <4 x i32> <i32 1, i32 3, i32 5, i32 7>
  %9 = shufflevector <4 x float> %2, <4 x float> %4, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  tail call cc 10 void %6(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %7, <4 x float> %8, <4 x float> %9) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @dotFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %lhs, <4 x float> %rhs, i32 127)
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @detMFloatX3(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in1, <4 x float> %in2, <4 x float> %in3) #0 {
  %1 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 0>
  %2 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %3 = fmul <4 x float> %1, %2
  %4 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %5 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 0>
  %6 = fmul <4 x float> %4, %5
  %7 = fsub <4 x float> %3, %6
  %8 = load i64* %sp, align 8, !tbaa !1
  %9 = inttoptr i64 %8 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %10 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %in1, <4 x float> %7, i32 255)
  tail call cc 10 void %9(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %10) #3
  ret void
}

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse.sqrt.ps(<4 x float>) #1

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse.rcp.ps(<4 x float>) #1

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse.cmp.ps(<4 x float>, <4 x float>, i8) #1

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse.min.ps(<4 x float>, <4 x float>) #1

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse.max.ps(<4 x float>, <4 x float>) #1

; Function Attrs: nounwind readnone
declare i32 @llvm.x86.sse.movmsk.ps(<4 x float>) #1

attributes #0 = { alwaysinline nounwind optsize uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { nounwind }
attributes #3 = { nounwind optsize }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.5.2-3ubuntu1 (tags/RELEASE_352/final) (based on LLVM 3.5.2)"}
!1 = metadata !{metadata !2, metadata !2, i64 0}
!2 = metadata !{metadata !"long", metadata !3, i64 0}
!3 = metadata !{metadata !"omnipotent char", metadata !4, i64 0}
!4 = metadata !{metadata !"Simple C/C++ TBAA"}
