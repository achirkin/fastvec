; ModuleID = 'prims/Int32X4Ops.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @ltInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = bitcast <2 x i64> %rhs to <4 x i32>
  %4 = bitcast <2 x i64> %lhs to <4 x i32>
  %5 = icmp sgt <4 x i32> %3, %4
  %6 = sext <4 x i1> %5 to <4 x i32>
  %7 = bitcast <4 x i32> %6 to <16 x i8>
  %8 = tail call i32 @llvm.x86.sse2.pmovmskb.128(<16 x i8> %7) #2
  %9 = sext i32 %8 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %9, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @gtInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = bitcast <2 x i64> %lhs to <4 x i32>
  %4 = bitcast <2 x i64> %rhs to <4 x i32>
  %5 = icmp sgt <4 x i32> %3, %4
  %6 = sext <4 x i1> %5 to <4 x i32>
  %7 = bitcast <4 x i32> %6 to <16 x i8>
  %8 = tail call i32 @llvm.x86.sse2.pmovmskb.128(<16 x i8> %7) #2
  %9 = sext i32 %8 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %9, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @eqInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*)*
  %3 = bitcast <2 x i64> %lhs to <4 x i32>
  %4 = bitcast <2 x i64> %rhs to <4 x i32>
  %5 = icmp eq <4 x i32> %3, %4
  %6 = sext <4 x i1> %5 to <4 x i32>
  %7 = bitcast <4 x i32> %6 to <16 x i8>
  %8 = tail call i32 @llvm.x86.sse2.pmovmskb.128(<16 x i8> %7) #2
  %9 = sext i32 %8 to i64
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 %9, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @maxInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  %3 = bitcast <2 x i64> %lhs to <4 x i32>
  %4 = bitcast <2 x i64> %rhs to <4 x i32>
  %5 = tail call <4 x i32> @llvm.x86.sse41.pmaxsd(<4 x i32> %3, <4 x i32> %4) #2
  %6 = bitcast <4 x i32> %5 to <2 x i64>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %6) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @minInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  %3 = bitcast <2 x i64> %lhs to <4 x i32>
  %4 = bitcast <2 x i64> %rhs to <4 x i32>
  %5 = tail call <4 x i32> @llvm.x86.sse41.pminsd(<4 x i32> %3, <4 x i32> %4) #2
  %6 = bitcast <4 x i32> %5 to <2 x i64>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %6) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @absInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  %3 = bitcast <2 x i64> %argvec to <4 x i32>
  %4 = tail call <4 x i32> @llvm.x86.sse2.psrai.d(<4 x i32> %3, i32 31) #2
  %5 = bitcast <4 x i32> %4 to <2 x i64>
  %6 = xor <2 x i64> %5, %argvec
  %7 = bitcast <2 x i64> %6 to <4 x i32>
  %8 = sub <4 x i32> %7, %4
  %9 = bitcast <4 x i32> %8 to <2 x i64>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %9) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @signumInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %argvec) #0 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  %3 = bitcast <2 x i64> %argvec to <4 x i32>
  %.lobit = ashr <4 x i32> %3, <i32 31, i32 31, i32 31, i32 31>
  %4 = icmp sgt <4 x i32> %3, zeroinitializer
  %5 = sext <4 x i1> %4 to <4 x i32>
  %6 = sub <4 x i32> %.lobit, %5
  %7 = bitcast <4 x i32> %6 to <2 x i64>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %7) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMMInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %in10, <2 x i64> %in11, <2 x i64> %in12, <2 x i64> %in13, <2 x i64> %in20, <2 x i64> %in21) #0 align 8 {
  %1 = bitcast i64* %sp to <2 x i64>*
  %2 = load <2 x i64>* %1, align 8, !tbaa !5
  %3 = getelementptr inbounds i64* %sp, i64 2
  %4 = bitcast i64* %3 to <2 x i64>*
  %5 = load <2 x i64>* %4, align 8, !tbaa !5
  %6 = getelementptr inbounds i64* %sp, i64 4
  %7 = bitcast <2 x i64> %in20 to <4 x i32>
  %8 = shufflevector <4 x i32> %7, <4 x i32> undef, <4 x i32> zeroinitializer
  %9 = shufflevector <4 x i32> %7, <4 x i32> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %10 = shufflevector <4 x i32> %7, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %11 = shufflevector <4 x i32> %7, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %12 = bitcast <2 x i64> %in10 to <4 x i32>
  %13 = mul <4 x i32> %12, %8
  %14 = bitcast <2 x i64> %in11 to <4 x i32>
  %15 = mul <4 x i32> %14, %9
  %16 = bitcast <2 x i64> %in12 to <4 x i32>
  %17 = mul <4 x i32> %16, %10
  %18 = bitcast <2 x i64> %in13 to <4 x i32>
  %19 = mul <4 x i32> %18, %11
  %20 = add <4 x i32> %13, %15
  %21 = add <4 x i32> %17, %19
  %22 = add <4 x i32> %20, %21
  %23 = bitcast <4 x i32> %22 to <2 x i64>
  %24 = bitcast <2 x i64> %in21 to <4 x i32>
  %25 = shufflevector <4 x i32> %24, <4 x i32> undef, <4 x i32> zeroinitializer
  %26 = shufflevector <4 x i32> %24, <4 x i32> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %27 = shufflevector <4 x i32> %24, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %28 = shufflevector <4 x i32> %24, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %29 = mul <4 x i32> %12, %25
  %30 = mul <4 x i32> %14, %26
  %31 = mul <4 x i32> %16, %27
  %32 = mul <4 x i32> %18, %28
  %33 = add <4 x i32> %29, %30
  %34 = add <4 x i32> %31, %32
  %35 = add <4 x i32> %33, %34
  %36 = bitcast <4 x i32> %35 to <2 x i64>
  %37 = bitcast <2 x i64> %2 to <4 x i32>
  %38 = shufflevector <4 x i32> %37, <4 x i32> undef, <4 x i32> zeroinitializer
  %39 = shufflevector <4 x i32> %37, <4 x i32> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %40 = shufflevector <4 x i32> %37, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %41 = shufflevector <4 x i32> %37, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %42 = mul <4 x i32> %12, %38
  %43 = mul <4 x i32> %14, %39
  %44 = mul <4 x i32> %16, %40
  %45 = mul <4 x i32> %18, %41
  %46 = add <4 x i32> %42, %43
  %47 = add <4 x i32> %44, %45
  %48 = add <4 x i32> %46, %47
  %49 = bitcast <4 x i32> %48 to <2 x i64>
  %50 = bitcast <2 x i64> %5 to <4 x i32>
  %51 = shufflevector <4 x i32> %50, <4 x i32> undef, <4 x i32> zeroinitializer
  %52 = shufflevector <4 x i32> %50, <4 x i32> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %53 = shufflevector <4 x i32> %50, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %54 = shufflevector <4 x i32> %50, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %55 = mul <4 x i32> %12, %51
  %56 = mul <4 x i32> %14, %52
  %57 = mul <4 x i32> %16, %53
  %58 = mul <4 x i32> %18, %54
  %59 = add <4 x i32> %55, %56
  %60 = add <4 x i32> %57, %58
  %61 = add <4 x i32> %59, %60
  %62 = bitcast <4 x i32> %61 to <2 x i64>
  %63 = load i64* %6, align 8, !tbaa !1
  %64 = inttoptr i64 %63 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>)*
  tail call cc 10 void %64(i64* %baseReg, i64* %6, i64* %hp, i64 %r1, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %23, <2 x i64> %36, <2 x i64> %49, <2 x i64> %62) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @prodMVInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %m1, <2 x i64> %m2, <2 x i64> %m3, <2 x i64> %m4, <2 x i64> %vec) #0 align 8 {
  %1 = load i64* %sp, align 8, !tbaa !1
  %2 = inttoptr i64 %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  %3 = bitcast <2 x i64> %vec to <4 x i32>
  %4 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> zeroinitializer
  %5 = bitcast <2 x i64> %m1 to <4 x i32>
  %6 = mul <4 x i32> %5, %4
  %7 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 1, i32 1, i32 1, i32 1>
  %8 = bitcast <2 x i64> %m2 to <4 x i32>
  %9 = mul <4 x i32> %8, %7
  %10 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 2, i32 2>
  %11 = bitcast <2 x i64> %m3 to <4 x i32>
  %12 = mul <4 x i32> %11, %10
  %13 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 3>
  %14 = bitcast <2 x i64> %m4 to <4 x i32>
  %15 = mul <4 x i32> %14, %13
  %16 = add <4 x i32> %12, %15
  %17 = add <4 x i32> %9, %16
  %18 = add <4 x i32> %6, %17
  %19 = bitcast <4 x i32> %18 to <2 x i64>
  tail call cc 10 void %2(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %19) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @transposeMInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %in0, <2 x i64> %in1, <2 x i64> %in2, <2 x i64> %in3) #0 {
  %1 = bitcast <2 x i64> %in0 to <4 x i32>
  %2 = bitcast <2 x i64> %in1 to <4 x i32>
  %3 = shufflevector <4 x i32> %1, <4 x i32> %2, <4 x i32> <i32 0, i32 4, i32 1, i32 5>
  %4 = bitcast <4 x i32> %3 to <2 x i64>
  %5 = bitcast <2 x i64> %in2 to <4 x i32>
  %6 = bitcast <2 x i64> %in3 to <4 x i32>
  %7 = shufflevector <4 x i32> %5, <4 x i32> %6, <4 x i32> <i32 0, i32 4, i32 1, i32 5>
  %8 = bitcast <4 x i32> %7 to <2 x i64>
  %9 = shufflevector <4 x i32> %1, <4 x i32> %2, <4 x i32> <i32 2, i32 6, i32 3, i32 7>
  %10 = bitcast <4 x i32> %9 to <2 x i64>
  %11 = shufflevector <4 x i32> %5, <4 x i32> %6, <4 x i32> <i32 2, i32 6, i32 3, i32 7>
  %12 = bitcast <4 x i32> %11 to <2 x i64>
  %13 = load i64* %sp, align 8, !tbaa !1
  %14 = inttoptr i64 %13 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>)*
  %15 = shufflevector <2 x i64> %4, <2 x i64> %8, <2 x i32> <i32 0, i32 2>
  %16 = shufflevector <2 x i64> %4, <2 x i64> %8, <2 x i32> <i32 1, i32 3>
  %17 = shufflevector <2 x i64> %10, <2 x i64> %12, <2 x i32> <i32 0, i32 2>
  %18 = shufflevector <2 x i64> %10, <2 x i64> %12, <2 x i32> <i32 1, i32 3>
  tail call cc 10 void %14(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %15, <2 x i64> %16, <2 x i64> %17, <2 x i64> %18) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @dotInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %lhs, <2 x i64> %rhs) #0 {
  %1 = bitcast <2 x i64> %lhs to <4 x i32>
  %2 = bitcast <2 x i64> %rhs to <4 x i32>
  %3 = mul <4 x i32> %1, %2
  %4 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 2, i32 3, i32 0, i32 1>
  %5 = add <4 x i32> %3, %4
  %6 = shufflevector <4 x i32> %5, <4 x i32> undef, <4 x i32> <i32 1, i32 0, i32 3, i32 2>
  %7 = add <4 x i32> %5, %6
  %8 = bitcast <4 x i32> %7 to <2 x i64>
  %9 = load i64* %sp, align 8, !tbaa !1
  %10 = inttoptr i64 %9 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  tail call cc 10 void %10(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %8) #3
  ret void
}

; Function Attrs: alwaysinline nounwind optsize uwtable
define cc 10 void @detMInt32X4(i64* noalias %baseReg, i64* noalias %sp, i64* noalias %hp, i64 %r1, i64 %r2, i64 %r3, i64 %r4, i64 %r5, i64 %r6, i64* noalias %spLim, <2 x i64> %in1, <2 x i64> %in2, <2 x i64> %in3, <2 x i64> %in4) #0 {
  %1 = bitcast <2 x i64> %in3 to <4 x i32>
  %2 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 1>
  %3 = bitcast <2 x i64> %in4 to <4 x i32>
  %4 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %5 = mul <4 x i32> %2, %4
  %6 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 2, i32 0, i32 1, i32 0>
  %7 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 1, i32 2, i32 0, i32 1>
  %8 = mul <4 x i32> %6, %7
  %9 = sub <4 x i32> %5, %8
  %10 = bitcast <2 x i64> %in2 to <4 x i32>
  %11 = shufflevector <4 x i32> %10, <4 x i32> undef, <4 x i32> <i32 3, i32 3, i32 3, i32 2>
  %12 = mul <4 x i32> %11, %9
  %13 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 3, i32 0, i32 3, i32 0>
  %14 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 1, i32 3, i32 0, i32 2>
  %15 = mul <4 x i32> %13, %14
  %16 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 1, i32 3, i32 0, i32 2>
  %17 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 3, i32 0, i32 3, i32 0>
  %18 = mul <4 x i32> %16, %17
  %19 = sub <4 x i32> %15, %18
  %20 = shufflevector <4 x i32> %10, <4 x i32> undef, <4 x i32> <i32 2, i32 2, i32 1, i32 1>
  %21 = mul <4 x i32> %20, %19
  %22 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 2, i32 3, i32 1, i32 2>
  %23 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 3, i32 2, i32 3, i32 1>
  %24 = mul <4 x i32> %22, %23
  %25 = shufflevector <4 x i32> %1, <4 x i32> undef, <4 x i32> <i32 3, i32 2, i32 3, i32 1>
  %26 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> <i32 2, i32 3, i32 1, i32 2>
  %27 = mul <4 x i32> %25, %26
  %28 = sub <4 x i32> %24, %27
  %29 = shufflevector <4 x i32> %10, <4 x i32> undef, <4 x i32> <i32 1, i32 0, i32 0, i32 0>
  %30 = mul <4 x i32> %29, %28
  %31 = add <4 x i32> %12, %21
  %32 = add <4 x i32> %31, %30
  %33 = bitcast <2 x i64> %in1 to <4 x i32>
  %34 = mul <4 x i32> %33, %32
  %35 = shufflevector <4 x i32> %34, <4 x i32> undef, <4 x i32> <i32 2, i32 3, i32 0, i32 1>
  %36 = add <4 x i32> %34, %35
  %37 = shufflevector <4 x i32> %36, <4 x i32> undef, <4 x i32> <i32 1, i32 0, i32 3, i32 2>
  %38 = add <4 x i32> %36, %37
  %39 = bitcast <4 x i32> %38 to <2 x i64>
  %40 = load i64* %sp, align 8, !tbaa !1
  %41 = inttoptr i64 %40 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64*, <2 x i64>)*
  tail call cc 10 void %41(i64* %baseReg, i64* %sp, i64* %hp, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64* %spLim, <2 x i64> %39) #3
  ret void
}

; Function Attrs: nounwind readnone
declare <4 x i32> @llvm.x86.sse2.psrai.d(<4 x i32>, i32) #1

; Function Attrs: nounwind readnone
declare <4 x i32> @llvm.x86.sse41.pminsd(<4 x i32>, <4 x i32>) #1

; Function Attrs: nounwind readnone
declare <4 x i32> @llvm.x86.sse41.pmaxsd(<4 x i32>, <4 x i32>) #1

; Function Attrs: nounwind readnone
declare i32 @llvm.x86.sse2.pmovmskb.128(<16 x i8>) #1

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
