#include <stdlib.h>
#include <stdint.h>
#include <immintrin.h>


typedef void (*HsCallInt)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*);
typedef void (*HsCallFloatVec)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128);
typedef void (*HsCallFloatMat)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t*, __m128, __m128, __m128, __m128);

void __attribute__((always_inline)) ltFloatX4(
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
            _mm_movemask_ps(_mm_cmplt_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) gtFloatX4(
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
            _mm_movemask_ps(_mm_cmpgt_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) leFloatX4(
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
            _mm_movemask_ps(_mm_cmple_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) geFloatX4(
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
            _mm_movemask_ps(_mm_cmpge_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) eqFloatX4(
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
            _mm_movemask_ps(_mm_cmpeq_ps(lhs, rhs))
            ,iUndef,iUndef,iUndef,iUndef,iUndef,spLim            
            );
}

void __attribute__((always_inline)) maxFloatX4(
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

void __attribute__((always_inline)) minFloatX4(
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


void __attribute__((always_inline)) absFloatX4(
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

void __attribute__((always_inline)) signumFloatX4(
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

void __attribute__((always_inline)) recipFloatX4(
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

void __attribute__((always_inline)) sqrtFloatX4(
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

void __attribute__((always_inline)) inverseMFloatX4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in0,
    __m128 in1,
    __m128 in2,
    __m128 in3)
{
    __m128 Fac0;
    {
        //  valType SubFactor00 = m[2][2] * m[3][3] - m[3][2] * m[2][3];
        //  valType SubFactor00 = m[2][2] * m[3][3] - m[3][2] * m[2][3];
        //  valType SubFactor06 = m[1][2] * m[3][3] - m[3][2] * m[1][3];
        //  valType SubFactor13 = m[1][2] * m[2][3] - m[2][2] * m[1][3];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(3, 3, 3, 3));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac0 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }

    __m128 Fac1;
    {
        //  valType SubFactor01 = m[2][1] * m[3][3] - m[3][1] * m[2][3];
        //  valType SubFactor01 = m[2][1] * m[3][3] - m[3][1] * m[2][3];
        //  valType SubFactor07 = m[1][1] * m[3][3] - m[3][1] * m[1][3];
        //  valType SubFactor14 = m[1][1] * m[2][3] - m[2][1] * m[1][3];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(3, 3, 3, 3));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(1, 1, 1, 1));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac1 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }


    __m128 Fac2;
    {
        //  valType SubFactor02 = m[2][1] * m[3][2] - m[3][1] * m[2][2];
        //  valType SubFactor02 = m[2][1] * m[3][2] - m[3][1] * m[2][2];
        //  valType SubFactor08 = m[1][1] * m[3][2] - m[3][1] * m[1][2];
        //  valType SubFactor15 = m[1][1] * m[2][2] - m[2][1] * m[1][2];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(1, 1, 1, 1));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac2 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }

    __m128 Fac3;
    {
        //  valType SubFactor03 = m[2][0] * m[3][3] - m[3][0] * m[2][3];
        //  valType SubFactor03 = m[2][0] * m[3][3] - m[3][0] * m[2][3];
        //  valType SubFactor09 = m[1][0] * m[3][3] - m[3][0] * m[1][3];
        //  valType SubFactor16 = m[1][0] * m[2][3] - m[2][0] * m[1][3];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(3, 3, 3, 3));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(0, 0, 0, 0));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac3 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }

    __m128 Fac4;
    {
        //  valType SubFactor04 = m[2][0] * m[3][2] - m[3][0] * m[2][2];
        //  valType SubFactor04 = m[2][0] * m[3][2] - m[3][0] * m[2][2];
        //  valType SubFactor10 = m[1][0] * m[3][2] - m[3][0] * m[1][2];
        //  valType SubFactor17 = m[1][0] * m[2][2] - m[2][0] * m[1][2];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(0, 0, 0, 0));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(2, 2, 2, 2));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac4 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }

    __m128 Fac5;
    {
        //  valType SubFactor05 = m[2][0] * m[3][1] - m[3][0] * m[2][1];
        //  valType SubFactor05 = m[2][0] * m[3][1] - m[3][0] * m[2][1];
        //  valType SubFactor12 = m[1][0] * m[3][1] - m[3][0] * m[1][1];
        //  valType SubFactor18 = m[1][0] * m[2][1] - m[2][0] * m[1][1];

        __m128 Swp0a = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 Swp0b = _mm_shuffle_ps(in3, in2, _MM_SHUFFLE(0, 0, 0, 0));

        __m128 Swp00 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 Swp01 = _mm_shuffle_ps(Swp0a, Swp0a, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp02 = _mm_shuffle_ps(Swp0b, Swp0b, _MM_SHUFFLE(2, 0, 0, 0));
        __m128 Swp03 = _mm_shuffle_ps(in2, in1, _MM_SHUFFLE(1, 1, 1, 1));

        __m128 Mul00 = _mm_mul_ps(Swp00, Swp01);
        __m128 Mul01 = _mm_mul_ps(Swp02, Swp03);
        Fac5 = _mm_sub_ps(Mul00, Mul01);

        //bool stop = true;
    }

    __m128 SignA = _mm_set_ps( 1.0f,-1.0f, 1.0f,-1.0f);
    __m128 SignB = _mm_set_ps(-1.0f, 1.0f,-1.0f, 1.0f);

    // m[1][0]
    // m[0][0]
    // m[0][0]
    // m[0][0]
    __m128 Temp0 = _mm_shuffle_ps(in1, in0, _MM_SHUFFLE(0, 0, 0, 0));
    __m128 Vec0 = _mm_shuffle_ps(Temp0, Temp0, _MM_SHUFFLE(2, 2, 2, 0));

    // m[1][1]
    // m[0][1]
    // m[0][1]
    // m[0][1]
    __m128 Temp1 = _mm_shuffle_ps(in1, in0, _MM_SHUFFLE(1, 1, 1, 1));
    __m128 Vec1 = _mm_shuffle_ps(Temp1, Temp1, _MM_SHUFFLE(2, 2, 2, 0));

    // m[1][2]
    // m[0][2]
    // m[0][2]
    // m[0][2]
    __m128 Temp2 = _mm_shuffle_ps(in1, in0, _MM_SHUFFLE(2, 2, 2, 2));
    __m128 Vec2 = _mm_shuffle_ps(Temp2, Temp2, _MM_SHUFFLE(2, 2, 2, 0));

    // m[1][3]
    // m[0][3]
    // m[0][3]
    // m[0][3]
    __m128 Temp3 = _mm_shuffle_ps(in1, in0, _MM_SHUFFLE(3, 3, 3, 3));
    __m128 Vec3 = _mm_shuffle_ps(Temp3, Temp3, _MM_SHUFFLE(2, 2, 2, 0));

    // col0
    // + (Vec1[0] * Fac0[0] - Vec2[0] * Fac1[0] + Vec3[0] * Fac2[0]),
    // - (Vec1[1] * Fac0[1] - Vec2[1] * Fac1[1] + Vec3[1] * Fac2[1]),
    // + (Vec1[2] * Fac0[2] - Vec2[2] * Fac1[2] + Vec3[2] * Fac2[2]),
    // - (Vec1[3] * Fac0[3] - Vec2[3] * Fac1[3] + Vec3[3] * Fac2[3]),
    __m128 Mul00 = _mm_mul_ps(Vec1, Fac0);
    __m128 Mul01 = _mm_mul_ps(Vec2, Fac1);
    __m128 Mul02 = _mm_mul_ps(Vec3, Fac2);
    __m128 Sub00 = _mm_sub_ps(Mul00, Mul01);
    __m128 Add00 = _mm_add_ps(Sub00, Mul02);
    __m128 Inv0 = _mm_mul_ps(SignB, Add00);

    // col1
    // - (Vec0[0] * Fac0[0] - Vec2[0] * Fac3[0] + Vec3[0] * Fac4[0]),
    // + (Vec0[0] * Fac0[1] - Vec2[1] * Fac3[1] + Vec3[1] * Fac4[1]),
    // - (Vec0[0] * Fac0[2] - Vec2[2] * Fac3[2] + Vec3[2] * Fac4[2]),
    // + (Vec0[0] * Fac0[3] - Vec2[3] * Fac3[3] + Vec3[3] * Fac4[3]),
    __m128 Mul03 = _mm_mul_ps(Vec0, Fac0);
    __m128 Mul04 = _mm_mul_ps(Vec2, Fac3);
    __m128 Mul05 = _mm_mul_ps(Vec3, Fac4);
    __m128 Sub01 = _mm_sub_ps(Mul03, Mul04);
    __m128 Add01 = _mm_add_ps(Sub01, Mul05);
    __m128 Inv1 = _mm_mul_ps(SignA, Add01);

    // col2
    // + (Vec0[0] * Fac1[0] - Vec1[0] * Fac3[0] + Vec3[0] * Fac5[0]),
    // - (Vec0[0] * Fac1[1] - Vec1[1] * Fac3[1] + Vec3[1] * Fac5[1]),
    // + (Vec0[0] * Fac1[2] - Vec1[2] * Fac3[2] + Vec3[2] * Fac5[2]),
    // - (Vec0[0] * Fac1[3] - Vec1[3] * Fac3[3] + Vec3[3] * Fac5[3]),
    __m128 Mul06 = _mm_mul_ps(Vec0, Fac1);
    __m128 Mul07 = _mm_mul_ps(Vec1, Fac3);
    __m128 Mul08 = _mm_mul_ps(Vec3, Fac5);
    __m128 Sub02 = _mm_sub_ps(Mul06, Mul07);
    __m128 Add02 = _mm_add_ps(Sub02, Mul08);
    __m128 Inv2 = _mm_mul_ps(SignB, Add02);

    // col3
    // - (Vec1[0] * Fac2[0] - Vec1[0] * Fac4[0] + Vec2[0] * Fac5[0]),
    // + (Vec1[0] * Fac2[1] - Vec1[1] * Fac4[1] + Vec2[1] * Fac5[1]),
    // - (Vec1[0] * Fac2[2] - Vec1[2] * Fac4[2] + Vec2[2] * Fac5[2]),
    // + (Vec1[0] * Fac2[3] - Vec1[3] * Fac4[3] + Vec2[3] * Fac5[3]));
    __m128 Mul09 = _mm_mul_ps(Vec0, Fac2);
    __m128 Mul10 = _mm_mul_ps(Vec1, Fac4);
    __m128 Mul11 = _mm_mul_ps(Vec2, Fac5);
    __m128 Sub03 = _mm_sub_ps(Mul09, Mul10);
    __m128 Add03 = _mm_add_ps(Sub03, Mul11);
    __m128 Inv3 = _mm_mul_ps(SignA, Add03);

    __m128 Row0 = _mm_shuffle_ps(Inv0, Inv1, _MM_SHUFFLE(0, 0, 0, 0));
    __m128 Row1 = _mm_shuffle_ps(Inv2, Inv3, _MM_SHUFFLE(0, 0, 0, 0));
    __m128 Row2 = _mm_shuffle_ps(Row0, Row1, _MM_SHUFFLE(2, 0, 2, 0));

    //  valType Determinant = m[0][0] * Inverse[0][0]
    //                      + m[0][1] * Inverse[1][0]
    //                      + m[0][2] * Inverse[2][0]
    //                      + m[0][3] * Inverse[3][0];
    __m128 Det0 = _mm_dp_ps(in0, Row2, 0xFF);
    __m128 Rcp0 = _mm_div_ps(_mm_set1_ps(1.0f), Det0);
    //__m128 Rcp0 = _mm_rcp_ps(Det0);


    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_mul_ps(Inv0, Rcp0), //  Inverse /= Determinant;
            _mm_mul_ps(Inv1, Rcp0), //  Inverse /= Determinant;
            _mm_mul_ps(Inv2, Rcp0), //  Inverse /= Determinant;
            _mm_mul_ps(Inv3, Rcp0) //  Inverse /= Determinant;
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMMFloatX4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in10,
    __m128 in11,
    __m128 in12,
    __m128 in13,
    __m128 in20,
    __m128 in21)
{
    __m128* pointers = (__m128*)sp;
    __m128 out0, out1, out2, out3;
    __m128 in22 = *((__m128*)sp);
    __m128 in23 = *((__m128*)&sp[2]);

    int64_t* sp_mod = &sp[4];// (int64_t*)(&pointers[2]);
    {
        __m128 e0 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 e3 = _mm_shuffle_ps(in20, in20, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);
        __m128 m3 = _mm_mul_ps(in13, e3);

        __m128 a0 = _mm_add_ps(m0, m1);
        __m128 a1 = _mm_add_ps(m2, m3);
        __m128 a2 = _mm_add_ps(a0, a1);

        out0 = a2;
    }

    {
        __m128 e0 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 e3 = _mm_shuffle_ps(in21, in21, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);
        __m128 m3 = _mm_mul_ps(in13, e3);

        __m128 a0 = _mm_add_ps(m0, m1);
        __m128 a1 = _mm_add_ps(m2, m3);
        __m128 a2 = _mm_add_ps(a0, a1);

        out1 = a2;
    }

    {
        __m128 e0 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 e3 = _mm_shuffle_ps(in22, in22, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);
        __m128 m3 = _mm_mul_ps(in13, e3);

        __m128 a0 = _mm_add_ps(m0, m1);
        __m128 a1 = _mm_add_ps(m2, m3);
        __m128 a2 = _mm_add_ps(a0, a1);

        out2 = a2;
    }

    {
        //(__m128&)_mm_shuffle_epi32(__m128i&)in20, _MM_SHUFFLE(3, 3, 3, 3))
        __m128 e0 = _mm_shuffle_ps(in23, in23, _MM_SHUFFLE(0, 0, 0, 0));
        __m128 e1 = _mm_shuffle_ps(in23, in23, _MM_SHUFFLE(1, 1, 1, 1));
        __m128 e2 = _mm_shuffle_ps(in23, in23, _MM_SHUFFLE(2, 2, 2, 2));
        __m128 e3 = _mm_shuffle_ps(in23, in23, _MM_SHUFFLE(3, 3, 3, 3));

        __m128 m0 = _mm_mul_ps(in10, e0);
        __m128 m1 = _mm_mul_ps(in11, e1);
        __m128 m2 = _mm_mul_ps(in12, e2);
        __m128 m3 = _mm_mul_ps(in13, e3);

        __m128 a0 = _mm_add_ps(m0, m1);
        __m128 a1 = _mm_add_ps(m2, m3);
        __m128 a2 = _mm_add_ps(a0, a1);

        out3 = a2;
    }
    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp_mod[0];
    const float fUndef;
    return fun(
            baseReg,sp_mod,hp,
            r1,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            out0,out1,out2,out3
            );
}

void __attribute__((always_inline)) __attribute__((aligned(8))) prodMVFloatX4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 m1,
    __m128 m2,
    __m128 m3,
    __m128 m4,
    __m128 vec)
{
    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_add_ps(_mm_mul_ps(m1, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(0, 0, 0, 0))),
            _mm_add_ps(_mm_mul_ps(m2, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(1, 1, 1, 1))),
            _mm_add_ps(_mm_mul_ps(m3, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(2, 2, 2, 2))),
                       _mm_mul_ps(m4, _mm_shuffle_ps(vec, vec, _MM_SHUFFLE(3, 3, 3, 3))))))
            );
}



void __attribute__((always_inline)) transposeMFloatX4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in0,
    __m128 in1,
    __m128 in2,
    __m128 in3)
{
    __m128 tmp3, tmp2, tmp1, tmp0;
    tmp0 = _mm_shuffle_ps(in0, in1, 0x44);
    tmp2 = _mm_shuffle_ps(in0, in1, 0xEE);
    tmp1 = _mm_shuffle_ps(in2, in3, 0x44);
    tmp3 = _mm_shuffle_ps(in2, in3, 0xEE);

    const int64_t iUndef;
    const HsCallFloatMat fun = (HsCallFloatMat)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_shuffle_ps(tmp0, tmp1, 0x88),
            _mm_shuffle_ps(tmp0, tmp1, 0xDD),
            _mm_shuffle_ps(tmp2, tmp3, 0x88),
            _mm_shuffle_ps(tmp2, tmp3, 0xDD)
            );
}

void __attribute__((always_inline)) dotFloatX4(
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
            _mm_dp_ps(lhs, rhs, 0xFF)
            );
}

void __attribute__((always_inline)) detMFloatX4(
    int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp,
    int64_t r1, int64_t r2, int64_t r3, int64_t r4, int64_t r5, int64_t r6,
    int64_t* restrict spLim,
    __m128 in1,
    __m128 in2,
    __m128 in3,
    __m128 in4)
{
    __m128 p,q,a,b,o1,o2,o3;

    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(1, 0, 2, 1));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(0, 1, 0, 2));
    a = _mm_mul_ps(p,q);
    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(0, 1, 0, 2));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(1, 0, 2, 1));
    b = _mm_mul_ps(p,q);
    q = _mm_sub_ps(a,b);
    p = _mm_shuffle_ps(in2, in2, _MM_SHUFFLE(2, 3, 3, 3));
    o1 = _mm_mul_ps(p,q);
    
    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(0, 3, 0, 3));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(2, 0, 3, 1));
    a = _mm_mul_ps(p,q);
    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(2, 0, 3, 1));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(0, 3, 0, 3));
    b = _mm_mul_ps(p,q);
    q = _mm_sub_ps(a,b);
    p = _mm_shuffle_ps(in2, in2, _MM_SHUFFLE(1, 1, 2, 2));
    o2 = _mm_mul_ps(p,q);
    
    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(2, 1, 3, 2));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(1, 3, 2, 3));
    a = _mm_mul_ps(p,q);
    p = _mm_shuffle_ps(in3, in3, _MM_SHUFFLE(1, 3, 2, 3));
    q = _mm_shuffle_ps(in4, in4, _MM_SHUFFLE(2, 1, 3, 2));
    b = _mm_mul_ps(p,q);
    q = _mm_sub_ps(a,b);
    p = _mm_shuffle_ps(in2, in2, _MM_SHUFFLE(0, 0, 0, 1));
    o3 = _mm_mul_ps(p,q);

    p = _mm_add_ps(o1,o2);
    a = _mm_add_ps(p ,o3);

    const int64_t iUndef;
    const HsCallFloatVec fun = (HsCallFloatVec)sp[0];
    return fun(
            baseReg,sp,hp,
            iUndef,iUndef,iUndef,iUndef,iUndef,iUndef,spLim,
            _mm_dp_ps(in1, a, 0xFF)
            );
}
