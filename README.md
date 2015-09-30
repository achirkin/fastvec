# fastvec

This library is an attempt to make fast 2D-4D geometry library that highly relies on SIMD extentions.
The intention is to use the library in OpenGL programs.
At this stage only x86_64 architecture is supported.
All operations on vectors are imported from raw LLVM IR via "foreign import prim" mechanics.
Currently a lot of avx instructions are used, so it will only run on modern machines.
Vectors implement standard classes like Num, Fractional etc - all these operations are scalar;
for vector operations like dot products or matrix products I use separate classes.

