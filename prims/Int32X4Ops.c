#include <stdlib.h>
#include <stdint.h>
#include <immintrin.h>


typedef void (*HsCallInt)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*);
typedef void (*HsCallInt32Vec)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128i);
typedef void (*HsCallInt32Mat)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128i, __m128i, __m128i, __m128i);

void __attribute__((always_inline)) ltInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
             _mm_movemask_epi8(_mm_cmplt_epi32(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) gtInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
             _mm_movemask_epi8(_mm_cmpgt_epi32(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) eqInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    const int64_t iUndef;
    const HsCallInt fun = (HsCallInt)sp[0];
    return fun(
            baseReg,sp,hp,
            _mm_movemask_epi8(_mm_cmpeq_epi32(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) maxInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_max_epi32(lhs, rhs)
            );
}

void __attribute__((always_inline)) minInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_min_epi32(lhs, rhs)
            );
}


void __attribute__((always_inline)) absInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i argvec)
{
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    const __m128i b = _mm_srai_epi32(argvec, 31);
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_sub_epi32(_mm_xor_si128(b, argvec), b)
            );
}

void __attribute__((always_inline)) signumInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i argvec)
{
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_sub_epi32(_mm_cmpgt_epi32(_mm_setzero_si128(), argvec),
                         _mm_cmpgt_epi32(argvec, _mm_setzero_si128()))
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMMInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i in10,
    __m128i in11,
    __m128i in12,
    __m128i in13,
    __m128i in20,
    __m128i in21)
{
    __m128i* pointers = (__m128i*)sp;
    __m128i out0, out1, out2, out3;
    __m128i in22 = *((__m128i*)sp);
    __m128i in23 = *((__m128i*)&sp[2]);

    int64_t* sp_mod = &sp[4];// (int64_t*)(&pointers[2]);
    {
        __m128i e0 = _mm_shuffle_epi32( in20, _MM_SHUFFLE(0, 0, 0, 0));
        __m128i e1 = _mm_shuffle_epi32( in20, _MM_SHUFFLE(1, 1, 1, 1));
        __m128i e2 = _mm_shuffle_epi32( in20, _MM_SHUFFLE(2, 2, 2, 2));
        __m128i e3 = _mm_shuffle_epi32( in20, _MM_SHUFFLE(3, 3, 3, 3));

        __m128i m0 = _mm_mullo_epi32(in10, e0);
        __m128i m1 = _mm_mullo_epi32(in11, e1);
        __m128i m2 = _mm_mullo_epi32(in12, e2);
        __m128i m3 = _mm_mullo_epi32(in13, e3);

        __m128i a0 = _mm_add_epi32(m0, m1);
        __m128i a1 = _mm_add_epi32(m2, m3);
        __m128i a2 = _mm_add_epi32(a0, a1);

        out0 = a2;
    }

    {
        __m128i e0 = _mm_shuffle_epi32( in21, _MM_SHUFFLE(0, 0, 0, 0));
        __m128i e1 = _mm_shuffle_epi32( in21, _MM_SHUFFLE(1, 1, 1, 1));
        __m128i e2 = _mm_shuffle_epi32( in21, _MM_SHUFFLE(2, 2, 2, 2));
        __m128i e3 = _mm_shuffle_epi32( in21, _MM_SHUFFLE(3, 3, 3, 3));

        __m128i m0 = _mm_mullo_epi32(in10, e0);
        __m128i m1 = _mm_mullo_epi32(in11, e1);
        __m128i m2 = _mm_mullo_epi32(in12, e2);
        __m128i m3 = _mm_mullo_epi32(in13, e3);

        __m128i a0 = _mm_add_epi32(m0, m1);
        __m128i a1 = _mm_add_epi32(m2, m3);
        __m128i a2 = _mm_add_epi32(a0, a1);

        out1 = a2;
    }

    {
        __m128i e0 = _mm_shuffle_epi32( in22, _MM_SHUFFLE(0, 0, 0, 0));
        __m128i e1 = _mm_shuffle_epi32( in22, _MM_SHUFFLE(1, 1, 1, 1));
        __m128i e2 = _mm_shuffle_epi32( in22, _MM_SHUFFLE(2, 2, 2, 2));
        __m128i e3 = _mm_shuffle_epi32( in22, _MM_SHUFFLE(3, 3, 3, 3));

        __m128i m0 = _mm_mullo_epi32(in10, e0);
        __m128i m1 = _mm_mullo_epi32(in11, e1);
        __m128i m2 = _mm_mullo_epi32(in12, e2);
        __m128i m3 = _mm_mullo_epi32(in13, e3);

        __m128i a0 = _mm_add_epi32(m0, m1);
        __m128i a1 = _mm_add_epi32(m2, m3);
        __m128i a2 = _mm_add_epi32(a0, a1);

        out2 = a2;
    }

    {
        //(__m128i&)_mm_shuffle_epi32(__m128ii&)in20, _MM_SHUFFLE(3, 3, 3, 3))
        __m128i e0 = _mm_shuffle_epi32( in23, _MM_SHUFFLE(0, 0, 0, 0));
        __m128i e1 = _mm_shuffle_epi32( in23, _MM_SHUFFLE(1, 1, 1, 1));
        __m128i e2 = _mm_shuffle_epi32( in23, _MM_SHUFFLE(2, 2, 2, 2));
        __m128i e3 = _mm_shuffle_epi32( in23, _MM_SHUFFLE(3, 3, 3, 3));

        __m128i m0 = _mm_mullo_epi32(in10, e0);
        __m128i m1 = _mm_mullo_epi32(in11, e1);
        __m128i m2 = _mm_mullo_epi32(in12, e2);
        __m128i m3 = _mm_mullo_epi32(in13, e3);

        __m128i a0 = _mm_add_epi32(m0, m1);
        __m128i a1 = _mm_add_epi32(m2, m3);
        __m128i a2 = _mm_add_epi32(a0, a1);

        out3 = a2;
    }
    const int64_t iUndef;
    const HsCallInt32Mat fun = (HsCallInt32Mat)sp_mod[0];
    return fun(
            baseReg,sp_mod,hp,
            r1,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            out0,out1,out2,out3
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMVInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i m1,
    __m128i m2,
    __m128i m3,
    __m128i m4,
    __m128i vec)
{
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_add_epi32(_mm_mullo_epi32(m1, _mm_shuffle_epi32(vec, _MM_SHUFFLE(0, 0, 0, 0))),
            _mm_add_epi32(_mm_mullo_epi32(m2, _mm_shuffle_epi32(vec, _MM_SHUFFLE(1, 1, 1, 1))),
            _mm_add_epi32(_mm_mullo_epi32(m3, _mm_shuffle_epi32(vec, _MM_SHUFFLE(2, 2, 2, 2))),
                       _mm_mullo_epi32(m4, _mm_shuffle_epi32(vec, _MM_SHUFFLE(3, 3, 3, 3))))))
            );
}



void __attribute__((always_inline)) transposeMInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i in0,
    __m128i in1,
    __m128i in2,
    __m128i in3)
{
    __m128 tmp3, tmp2, tmp1, tmp0;
    tmp0 = _mm_unpacklo_epi32(in0, in1);
    tmp1 = _mm_unpacklo_epi32(in2, in3);
    tmp2 = _mm_unpackhi_epi32(in0, in1);
    tmp3 = _mm_unpackhi_epi32(in2, in3);

    const int64_t iUndef;
    const HsCallInt32Mat fun = (HsCallInt32Mat)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_unpacklo_epi64(tmp0, tmp1),
            _mm_unpackhi_epi64(tmp0, tmp1),
            _mm_unpacklo_epi64(tmp2, tmp3),
            _mm_unpackhi_epi64(tmp2, tmp3)
            );
}

void __attribute__((always_inline)) dotInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i lhs,
    __m128i rhs)
{
    __m128i r = _mm_mullo_epi32(lhs,rhs);
    r = _mm_add_epi32(r, _mm_shuffle_epi32(r, _MM_SHUFFLE(1, 0, 3, 2)));
    r = _mm_add_epi32(r, _mm_shuffle_epi32(r, _MM_SHUFFLE(2, 3, 0, 1)));
    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            r
            );
}

void __attribute__((always_inline)) detMInt32X4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128i in1,
    __m128i in2,
    __m128i in3,
    __m128i in4)
{
    __m128i p,q,a,b,o1,o2,o3;

    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(1, 0, 2, 1));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(0, 1, 0, 2));
    a = _mm_mullo_epi32(p,q);
    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(0, 1, 0, 2));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(1, 0, 2, 1));
    b = _mm_mullo_epi32(p,q);
    q = _mm_sub_epi32(a,b);
    p = _mm_shuffle_epi32(in2, _MM_SHUFFLE(2, 3, 3, 3));
    o1 = _mm_mullo_epi32(p,q);
    
    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(0, 3, 0, 3));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(2, 0, 3, 1));
    a = _mm_mullo_epi32(p,q);
    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(2, 0, 3, 1));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(0, 3, 0, 3));
    b = _mm_mullo_epi32(p,q);
    q = _mm_sub_epi32(a,b);
    p = _mm_shuffle_epi32(in2, _MM_SHUFFLE(1, 1, 2, 2));
    o2 = _mm_mullo_epi32(p,q);
    
    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(2, 1, 3, 2));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(1, 3, 2, 3));
    a = _mm_mullo_epi32(p,q);
    p = _mm_shuffle_epi32(in3, _MM_SHUFFLE(1, 3, 2, 3));
    q = _mm_shuffle_epi32(in4, _MM_SHUFFLE(2, 1, 3, 2));
    b = _mm_mullo_epi32(p,q);
    q = _mm_sub_epi32(a,b);
    p = _mm_shuffle_epi32(in2, _MM_SHUFFLE(0, 0, 0, 1));
    o3 = _mm_mullo_epi32(p,q);

    p = _mm_add_epi32(o1,o2);
    a = _mm_add_epi32(p ,o3);
    
    __m128i r = _mm_mullo_epi32(in1, a);
    r = _mm_add_epi32(r, _mm_shuffle_epi32(r, _MM_SHUFFLE(1, 0, 3, 2)));
    r = _mm_add_epi32(r, _mm_shuffle_epi32(r, _MM_SHUFFLE(2, 3, 0, 1)));

    const int64_t iUndef;
    const HsCallInt32Vec fun = (HsCallInt32Vec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            r
            );
}
