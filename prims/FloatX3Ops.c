#include <stdlib.h>
#include <stdint.h>
#include <immintrin.h>


typedef void (*HsCallInt)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*);
typedef void (*HsCallFloatVec)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128);
typedef void (*HsCallFloatMat)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128, __m128, __m128);

void __attribute__((always_inline)) ltFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            0x00000001 | _mm_movemask_ps(_mm_cmplt_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) gtFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            0x00000001 | _mm_movemask_ps(_mm_cmpgt_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) leFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            0x00000001 | _mm_movemask_ps(_mm_cmple_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) geFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            0x00000001 | _mm_movemask_ps(_mm_cmpge_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) eqFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            0x00000001 | _mm_movemask_ps(_mm_cmpeq_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) maxFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_max_ps(lhs, rhs)
            );
}

void __attribute__((always_inline)) minFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_min_ps(lhs, rhs)
            );
}


void __attribute__((always_inline)) absFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 argvec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_andnot_ps(_mm_set1_ps(-0.0f), argvec)
            );
}

void __attribute__((always_inline)) signumFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 argvec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_and_ps(_mm_or_ps(_mm_and_ps(argvec, _mm_set1_ps(-0.0f)),
				_mm_set1_ps(1.0f)),
			  _mm_cmpneq_ps(argvec, _mm_setzero_ps()))
            );
}

void __attribute__((always_inline)) recipFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 argvec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_rcp_ps(argvec)
            );
}

void __attribute__((always_inline)) sqrtFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 argvec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_sqrt_ps(argvec)
            );
}

void __attribute__((always_inline)) inverseMFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 c1,
    __m128 c2,
    __m128 c3)
{
    __m128 q, p, t;
    __m128 a1; // column 1 of adjugate matrix
    {
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(1, 2, 0, 1));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 2, 1, 2));
        t = _mm_mul_ps(p, q);
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(2, 1, 0, 2));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 1, 2, 1));
        a1 = _mm_sub_ps(t, _mm_mul_ps(p, q));
    }
    
    __m128 a2; // column 2 of adjugate matrix
    {
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(0, 2, 0, 0));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 2, 0, 2));
        t = _mm_mul_ps(p, q);
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(2, 0, 0, 2));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 0, 2, 0));
        a2 = _mm_sub_ps(_mm_mul_ps(p, q), t);
    }
    
    __m128 a3; // column 3 of adjugate matrix
    {
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(0, 1, 0, 0));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 1, 0, 1));
        t = _mm_mul_ps(p, q);
        p = _mm_shuffle_ps(c2, c1, _MM_SHUFFLE(1, 0, 0, 1));
        p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 3, 2, 0));
        q = _mm_shuffle_ps(c3, c2, _MM_SHUFFLE(0, 0, 1, 0));
        a3 = _mm_sub_ps(t, _mm_mul_ps(p, q));
    }
    p = _mm_shuffle_ps(c1, c2, _MM_SHUFFLE(0, 0, 0, 0));
    p = _mm_shuffle_ps(p, p, _MM_SHUFFLE(1, 0, 1, 0));
    q = _mm_shuffle_ps(p, c3, _MM_SHUFFLE(3, 2, 1, 0));
    __m128 rcpd = _mm_div_ps(_mm_set1_ps(1.0f), _mm_dp_ps(q, a1, 0x7F));
    

    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_mul_ps(a1, rcpd), //  Inverse /= Determinant;
            _mm_mul_ps(a2, rcpd), //  Inverse /= Determinant;
            _mm_mul_ps(a3, rcpd) //  Inverse /= Determinant;
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMMFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in10,
    __m128 in11,
    __m128 in12,
    __m128 in20,
    __m128 in21,
    __m128 in22)
{
    __m128 out0, out1, out2;
    {
        __m128 e0 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);

        __m128 a0 = _mm_add_ps(m0, m1);
        out0 = _mm_add_ps(a0, m2);
    }

    {
        __m128 e0 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);

        __m128 a0 = _mm_add_ps(m0, m1);
        out1 = _mm_add_ps(a0, m2);
    }

    {
        __m128 e0 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);

        __m128 a0 = _mm_add_ps(m0, m1);
        out2 = _mm_add_ps(a0, m2);
    }

    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp[0];
    const float fUndef;
    return fun(
            baseReg,sp,hp,
            r1,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            out0,out1,out2
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMVFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 m1,
    __m128 m2,
    __m128 m3,
    __m128 vec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_add_ps(_mm_mul_ps(m1, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(0, 0, 0, 0))),
            _mm_add_ps(_mm_mul_ps(m2, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(1, 1, 1, 1))),
                       _mm_mul_ps(m3, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(2, 2, 2, 2)))))
            );
}


void __attribute__((always_inline)) transposeMFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in0,
    __m128 in1,
    __m128 in2)
{
    __m128 tmp3, tmp2, tmp1, tmp0;
    tmp0 = _mm_shuffle_ps(in0, in1, 0x44);
    tmp2 = _mm_shuffle_ps(in0, in1, 0xEE);
    tmp1 = _mm_shuffle_ps(in2, in2, 0x44);
    tmp3 = _mm_shuffle_ps(in2, in2, 0xEE);

    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_shuffle_ps(tmp0, tmp1, 0x88),
            _mm_shuffle_ps(tmp0, tmp1, 0xDD),
            _mm_shuffle_ps(tmp2, tmp3, 0x88)
            );
}

void __attribute__((always_inline)) dotFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 lhs,
    __m128 rhs)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_dp_ps(lhs, rhs, 0x7F)
            );
}

void __attribute__((always_inline)) detMFloatX3(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in1,
    __m128 in2,
    __m128 in3)
{
    __m128 p,q,a,b;

    p = _mm_shuffle_ps(in2, in2, _MM_SHUFFLE(0, 0, 2, 1));
    q = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(0, 1, 0, 2));
    a = _mm_mul_ps(p,q);
    p = _mm_shuffle_ps(in2, in2, _MM_SHUFFLE(0, 1, 0, 2));
    q = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(0, 0, 2, 1));
    b = _mm_mul_ps(p,q);
    q = _mm_sub_ps(a,b);

    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_dp_ps(in1, q, 0xFF)
            );
}
