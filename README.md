# fastvec

This library is an attempt to make fast 2D-4D geometry library that highly relies on SIMD extentions.
The intention is to use the library in OpenGL programs.
At this stage only x86_64 architecture is supported.
All operations on vectors are imported from raw LLVM IR via "foreign import prim" mechanics.
Currently a lot of avx instructions are used, so it will only run on modern machines.
Vectors implement standard classes like Num, Fractional etc - all these operations are scalar;
for vector operations like dot products or matrix products I use separate classes.

### GHCJS support

The library is compilable with GHCJS 0.2 (unfortunately, without tests yet).
Additional tweaks have been made to run vector math more or less efficiently.

All vectors and matrices in GHCJS version of the library are just plane JavaScript arrays of numbers.
But in Haskell they are presented the same way as x86_64 SIMD vectors and matrices;
this makes it possible to write cross-platform x86_64-JS programs using the same data types.
I prefer the plane arrays over the typed arrays in order to be able to work with (Geo)JSON objects without any conversion of types: coordinate arrays in GeoJSON are exactly the same thing as vectors in Haskell!

No SIMD in JS yet (because SIMD.js is not yet ready, and because now I use plane arrays).
