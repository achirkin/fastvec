# fastvec

This library is an attempt to make fast 2D-4D geometry library that highly relies on SIMD extentions.
The intention is to use the library in OpenGL programs.
At this stage only x86_64 architecture is supported.
All operations on vectors are imported from raw LLVM IR via "foreign import prim" mechanics.
Currently a lot of avx instructions are used, so it will only run on modern machines.
Vectors implement standard classes like Num, Fractional etc - all these operations are scalar;
for vector operations like dot products or matrix products I use separate classes.

### GHCJS support

The library is compilable with GHCJS (Unfortunately, without tests yet).
Additional tweaks have been made to do this more or less efficiently.

All vectors and matrices in GHCJS version of library are just plane JavaScript arrays of numbers.
But in Haskell they presented the same way as x86 SIMD vectors and matrices;
this makes it possible to write cross-platform x64-js programs with the same vector representations.
I prefered plane arrays over typed arrays in order to be able to work with (Geo)JSON objects without any convertion of types: now coordinate arrays in GeoJSON are exactly the same thing as vectors in Haskell!
