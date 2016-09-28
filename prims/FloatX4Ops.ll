; ModuleID = 'prims/FloatX4Ops.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @ltFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 1) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = sext i32 %4 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @gtFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %rhs, <4 x float> %lhs, i8 1) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = sext i32 %4 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @leFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 2) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = sext i32 %4 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @geFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %rhs, <4 x float> %lhs, i8 2) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = sext i32 %4 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @eqFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = tail call <4 x float> @llvm.x86.sse.cmp.ps(<4 x float> %lhs, <4 x float> %rhs, i8 0) #2
  %4 = tail call i32 @llvm.x86.sse.movmsk.ps(<4 x float> %3) #2
  %5 = sext i32 %4 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @maxFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.max.ps(<4 x float> %lhs, <4 x float> %rhs) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @minFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.min.ps(<4 x float> %lhs, <4 x float> %rhs) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @absFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = bitcast <4 x float> %argvec to <4 x i32>
  %4 = and <4 x i32> %3, <i32 2147483647, i32 2147483647, i32 2147483647, i32 2147483647>
  %5 = bitcast <4 x i32> %4 to <4 x float>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %5) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @signumFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
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
define cc 10 void @recipFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.rcp.ps(<4 x float> %argvec) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @sqrtFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse.sqrt.ps(<4 x float> %argvec) #2
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @inverseMFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in0, <4 x float> %in1, <4 x float> %in2, <4 x float> %in3) #0 {
  %1 = shufflevector <4 x float> %in3, <4 x float> %in2, <4 x i32> <i32 3, i32 undef, i32 7, i32 undef>
  %2 = shufflevector <4 x float> %in3, <4 x float> %in2, <4 x i32> <i32 2, i32 undef, i32 6, i32 undef>
  %3 = shufflevector <4 x float> %in2, <4 x float> %in1, <4 x i32> <i32 2, i32 2, i32 6, i32 6>
  %4 = shufflevector <4 x float> %1, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 0, i32 2>
  %5 = shufflevector <4 x float> %2, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 0, i32 2>
  %6 = shufflevector <4 x float> %in2, <4 x float> %in1, <4 x i32> <i32 3, i32 3, i32 7, i32 7>
  %7 = fmul <4 x float> %3, %4
  %8 = fmul <4 x float> %6, %5
  %9 = fsub <4 x float> %7, %8
  %10 = shufflevector <4 x float> %in3, <4 x float> %in2, <4 x i32> <i32 1, i32 undef, i32 5, i32 undef>
  %11 = shufflevector <4 x float> %in2, <4 x float> %in1, <4 x i32> <i32 1, i32 1, i32 5, i32 5>
  %12 = shufflevector <4 x float> %10, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 0, i32 2>
  %13 = fmul <4 x float> %11, %4
  %14 = fmul <4 x float> %6, %12
  %15 = fsub <4 x float> %13, %14
  %16 = fmul <4 x float> %11, %5
  %17 = fmul <4 x float> %3, %12
  %18 = fsub <4 x float> %16, %17
  %19 = shufflevector <4 x float> %in3, <4 x float> %in2, <4 x i32> <i32 0, i32 undef, i32 4, i32 undef>
  %20 = shufflevector <4 x float> %in2, <4 x float> %in1, <4 x i32> <i32 0, i32 0, i32 4, i32 4>
  %21 = shufflevector <4 x float> %19, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 0, i32 2>
  %22 = fmul <4 x float> %20, %4
  %23 = fmul <4 x float> %6, %21
  %24 = fsub <4 x float> %22, %23
  %25 = fmul <4 x float> %20, %5
  %26 = fmul <4 x float> %3, %21
  %27 = fsub <4 x float> %25, %26
  %28 = fmul <4 x float> %20, %12
  %29 = fmul <4 x float> %11, %21
  %30 = fsub <4 x float> %28, %29
  %31 = shufflevector <4 x float> %in1, <4 x float> %in0, <4 x i32> <i32 0, i32 undef, i32 4, i32 undef>
  %32 = shufflevector <4 x float> %31, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 2, i32 2>
  %33 = shufflevector <4 x float> %in1, <4 x float> %in0, <4 x i32> <i32 1, i32 undef, i32 5, i32 undef>
  %34 = shufflevector <4 x float> %33, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 2, i32 2>
  %35 = shufflevector <4 x float> %in1, <4 x float> %in0, <4 x i32> <i32 2, i32 undef, i32 6, i32 undef>
  %36 = shufflevector <4 x float> %35, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 2, i32 2>
  %37 = shufflevector <4 x float> %in1, <4 x float> %in0, <4 x i32> <i32 3, i32 undef, i32 7, i32 undef>
  %38 = shufflevector <4 x float> %37, <4 x float> undef, <4 x i32> <i32 0, i32 2, i32 2, i32 2>
  %39 = fmul <4 x float> %34, %9
  %40 = fmul <4 x float> %36, %15
  %41 = fmul <4 x float> %38, %18
  %42 = fsub <4 x float> %39, %40
  %43 = fadd <4 x float> %41, %42
  %44 = fmul <4 x float> %43, <float 1.000000e+00, float -1.000000e+00, float 1.000000e+00, float -1.000000e+00>
  %45 = fmul <4 x float> %32, %9
  %46 = fmul <4 x float> %36, %24
  %47 = fmul <4 x float> %38, %27
  %48 = fsub <4 x float> %45, %46
  %49 = fadd <4 x float> %47, %48
  %50 = fmul <4 x float> %49, <float -1.000000e+00, float 1.000000e+00, float -1.000000e+00, float 1.000000e+00>
  %51 = fmul <4 x float> %32, %15
  %52 = fmul <4 x float> %34, %24
  %53 = fmul <4 x float> %38, %30
  %54 = fsub <4 x float> %51, %52
  %55 = fadd <4 x float> %53, %54
  %56 = fmul <4 x float> %55, <float 1.000000e+00, float -1.000000e+00, float 1.000000e+00, float -1.000000e+00>
  %57 = fmul <4 x float> %32, %18
  %58 = fmul <4 x float> %34, %27
  %59 = fmul <4 x float> %36, %30
  %60 = fsub <4 x float> %57, %58
  %61 = fadd <4 x float> %59, %60
  %62 = fmul <4 x float> %61, <float -1.000000e+00, float 1.000000e+00, float -1.000000e+00, float 1.000000e+00>
  %63 = shufflevector <4 x float> %44, <4 x float> %50, <4 x i32> <i32 0, i32 undef, i32 4, i32 undef>
  %64 = shufflevector <4 x float> %56, <4 x float> %62, <4 x i32> <i32 0, i32 undef, i32 4, i32 undef>
  %65 = shufflevector <4 x float> %63, <4 x float> %64, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %66 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %in0, <4 x float> %65, i32 255)
  %67 = fdiv <4 x float> <float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00>, %66
  %68 = load i64* %sp, align 8, !tbaa !1
  %69 = inttoptr i64 %68 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>, <4 x float>)*
  %70 = fmul <4 x float> %44, %67
  %71 = fmul <4 x float> %50, %67
  %72 = fmul <4 x float> %56, %67
  %73 = fmul <4 x float> %62, %67
  tail call cc 10 void %69(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %70, <4 x float> %71, <4 x float> %72, <4 x float> %73) #3
  ret void
}

; Function Attrs: nounwind readnone
declare <4 x float> @llvm.x86.sse41.dpps(<4 x float>, <4 x float>, i32) #1

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMMFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in10, <4 x float> %in11, <4 x float> %in12, <4 x float> %in13, <4 x float> %in20, <4 x float> %in21) #0 align 8 {
  %1 = bitcast i64* %sp to <4 x float>*
  %2 = load <4 x float>* %1, align 8, !tbaa !5
  %3 = getelementptr inbounds i64* %sp, i64 2
  %4 = bitcast i64* %3 to <4 x float>*
  %5 = load <4 x float>* %4, align 8, !tbaa !5
  %6 = getelementptr inbounds i64* %sp, i64 4
  %7 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> zeroinitializer
  %8 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %9 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %10 = shufflevector <4 x float> %in20, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %11 = fmul <4 x float> %7, %in10
  %12 = fmul <4 x float> %8, %in11
  %13 = fmul <4 x float> %9, %in12
  %14 = fmul <4 x float> %10, %in13
  %15 = fadd <4 x float> %11, %12
  %16 = fadd <4 x float> %13, %14
  %17 = fadd <4 x float> %15, %16
  %18 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> zeroinitializer
  %19 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %20 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %21 = shufflevector <4 x float> %in21, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %22 = fmul <4 x float> %18, %in10
  %23 = fmul <4 x float> %19, %in11
  %24 = fmul <4 x float> %20, %in12
  %25 = fmul <4 x float> %21, %in13
  %26 = fadd <4 x float> %22, %23
  %27 = fadd <4 x float> %24, %25
  %28 = fadd <4 x float> %26, %27
  %29 = shufflevector <4 x float> %2, <4 x float> undef, <4 x i32> zeroinitializer
  %30 = shufflevector <4 x float> %2, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %31 = shufflevector <4 x float> %2, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %32 = shufflevector <4 x float> %2, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %33 = fmul <4 x float> %29, %in10
  %34 = fmul <4 x float> %30, %in11
  %35 = fmul <4 x float> %31, %in12
  %36 = fmul <4 x float> %32, %in13
  %37 = fadd <4 x float> %33, %34
  %38 = fadd <4 x float> %35, %36
  %39 = fadd <4 x float> %37, %38
  %40 = shufflevector <4 x float> %5, <4 x float> undef, <4 x i32> zeroinitializer
  %41 = shufflevector <4 x float> %5, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %42 = shufflevector <4 x float> %5, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %43 = shufflevector <4 x float> %5, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %44 = fmul <4 x float> %40, %in10
  %45 = fmul <4 x float> %41, %in11
  %46 = fmul <4 x float> %42, %in12
  %47 = fmul <4 x float> %43, %in13
  %48 = fadd <4 x float> %44, %45
  %49 = fadd <4 x float> %46, %47
  %50 = fadd <4 x float> %48, %49
  %51 = load i64* %6, align 8, !tbaa !1
  %52 = inttoptr i64 %51 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>, <4 x float>)*
  tail call cc 10 void %52(i64* %baseReg, i64* %6, i64* %hp, i64 %r1, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %17, <4 x float> %28, <4 x float> %39, <4 x float> %50) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMVFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %m1, <4 x float> %m2, <4 x float> %m3, <4 x float> %m4, <4 x float> %vec) #0 align 8 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> zeroinitializer
  %4 = fmul <4 x float> %3, %m1
  %5 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %6 = fmul <4 x float> %5, %m2
  %7 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %8 = fmul <4 x float> %7, %m3
  %9 = shufflevector <4 x float> %vec, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %10 = fmul <4 x float> %9, %m4
  %11 = fadd <4 x float> %8, %10
  %12 = fadd <4 x float> %6, %11
  %13 = fadd <4 x float> %4, %12
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %13) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @transposeMFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in0, <4 x float> %in1, <4 x float> %in2, <4 x float> %in3) #0 {
  %1 = shufflevector <4 x float> %in0, <4 x float> %in1, <4 x i32> <i32 0, i32 1, i32 4, i32 5>
  %2 = shufflevector <4 x float> %in0, <4 x float> %in1, <4 x i32> <i32 2, i32 3, i32 6, i32 7>
  %3 = shufflevector <4 x float> %in2, <4 x float> %in3, <4 x i32> <i32 0, i32 1, i32 4, i32 5>
  %4 = shufflevector <4 x float> %in2, <4 x float> %in3, <4 x i32> <i32 2, i32 3, i32 6, i32 7>
  %5 = load i64* %sp, align 8, !tbaa !1
  %6 = inttoptr i64 %5 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>, <4 x float>, <4 x float>, <4 x float>)*
  %7 = shufflevector <4 x float> %1, <4 x float> %3, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %8 = shufflevector <4 x float> %1, <4 x float> %3, <4 x i32> <i32 1, i32 3, i32 5, i32 7>
  %9 = shufflevector <4 x float> %2, <4 x float> %4, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %10 = shufflevector <4 x float> %2, <4 x float> %4, <4 x i32> <i32 1, i32 3, i32 5, i32 7>
  tail call cc 10 void %6(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %7, <4 x float> %8, <4 x float> %9, <4 x float> %10) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @dotFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %lhs, <4 x float> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %3 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %lhs, <4 x float> %rhs, i32 255)
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %3) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @detMFloatX4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <4 x float> %in1, <4 x float> %in2, <4 x float> %in3, <4 x float> %in4) #0 {
  %1 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 1>
  %2 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %3 = fmul <4 x float> %1, %2
  %4 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %5 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 1>
  %6 = fmul <4 x float> %4, %5
  %7 = fsub <4 x float> %3, %6
  %8 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 2>
  %9 = fmul <4 x float> %8, %7
  %10 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 3, i32 0, i32 3, i32 0>
  %11 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 1, i32 3, i32 0, i32 2>
  %12 = fmul <4 x float> %10, %11
  %13 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 1, i32 3, i32 0, i32 2>
  %14 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 3, i32 0, i32 3, i32 0>
  %15 = fmul <4 x float> %13, %14
  %16 = fsub <4 x float> %12, %15
  %17 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 2, i32 2, i32 1, i32 1>
  %18 = fmul <4 x float> %17, %16
  %19 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 2, i32 3, i32 1, i32 2>
  %20 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 3, i32 2, i32 3, i32 1>
  %21 = fmul <4 x float> %19, %20
  %22 = shufflevector <4 x float> %in3, <4 x float> undef, <4 x i32> <i32 3, i32 2, i32 3, i32 1>
  %23 = shufflevector <4 x float> %in4, <4 x float> undef, <4 x i32> <i32 2, i32 3, i32 1, i32 2>
  %24 = fmul <4 x float> %22, %23
  %25 = fsub <4 x float> %21, %24
  %26 = shufflevector <4 x float> %in2, <4 x float> undef, <4 x i32> <i32 1, i32 0, i32 0, i32 0>
  %27 = fmul <4 x float> %26, %25
  %28 = fadd <4 x float> %9, %18
  %29 = fadd <4 x float> %27, %28
  %30 = load i64* %sp, align 8, !tbaa !1
  %31 = inttoptr i64 %30 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <4 x float>)*
  %32 = tail call <4 x float> @llvm.x86.sse41.dpps(<4 x float> %in1, <4 x float> %29, i32 255)
  tail call cc 10 void %31(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <4 x float> %32) #3
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
!5 = metadata !{metadata !3, metadata !3, i64 0}
