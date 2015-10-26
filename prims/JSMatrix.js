function cross(a,b){
    'use strict';
    return [ a[1]*b[2]-a[2]*b[1]
           , a[2]*b[0]-a[0]*b[2]
           , a[0]*b[1]-a[1]*b[0]
           ];
}

function translateM(v) {
    'use strict';
    return [ 1, 0, 0, 0
           , 0, 1, 0, 0
           , 0, 0, 1, 0
           , v[0], v[1], v[2], 1];
}

function rotateXM(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ 1, 0, 0, 0
           , 0, c, s, 0
           , 0,-s, c, 0
           , 0, 0, 0, 1];
}

function rotateYM(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ c, 0,-s, 0
           , 0, 1, 0, 0
           , s, 0, c, 0
           , 0, 0, 0, 0];
}

function rotateZM(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ c, s, 0, 0
           ,-s, c, 0, 0
           , 0, 0, 1, 0
           , 0, 0, 0, 1];
}

function rotateM(vec, a) {
    'use strict';
    var c = Math.cos(a);
    var s = Math.sin(a);
    var c1 = 1 - c;
    var x = vec[0],  y = vec[1],  z = vec[2];
    return [   c + c1*x*x, c1*x*y + s*z, c1*x*z - s*y, 0
           , c1*x*y - s*z,   c + c1*y*y, c1*y*z + s*x, 0
           , c1*x*z + s*y, c1*y*z - s*x,  c  + c1*z*z, 0
           , 0, 0, 0, 1];
}

function rotateEulerM(x, y, z) {
    'use strict';
    var cx = Math.cos(x), sx = Math.sin(x), cy = Math.cos(y), sy = Math.sin(y), cz = Math.cos(z), sz = Math.sin(z);
    return [            cy*cz,           -cy*sz,     sy, 0
           , cx*sz + sx*sy*cz, cx*cz - sx*sy*sz, -sx*cy, 0
           , sx*sz - cx*sy*cz, sx*cz + cx*sy*sz,  cx*cy, 0
           , 0, 0, 0, 1];
}

function fromQuaternion(quat) {
    'use strict';
    var x = quat[0], y = quat[1], z = quat[2], c = quat[3];
    if (x === 0 && y === 0 && z === 0) {
        var r = c*c;
        return [r,0,0,0
               ,0,r,0,0
               ,0,0,r,0
               ,0,0,0,1];
    }
    var l = Math.hypot(x,y,z,c);
    var c1 = l/(l+c);
    return [ l*c + c1*x*x, c1*x*y + l*z, c1*x*z - l*y, 0
           , c1*x*y - l*z, l*c + c1*y*y, c1*y*z + l*x, 0
           , c1*x*z + l*y, c1*y*z - l*x, l*c + c1*z*z, 0
           , 0, 0, 0, 1];
}

function lookAtMatrix(up,camera,point) {
    'use strict';
    var zDir = [camera[0]-point[0],camera[1]-point[1],camera[2]-point[2]];
    var t = Math.hypot.apply(null,zDir);
    zDir = zDir.map(function (e){return e / t;});
    var xDir = cross(up,zDir);
    t = Math.hypot.apply(null,xDir);
    xDir = xDir.map(function (e){return e / t;});
    var yDir = cross(zDir,xDir);
    return [ xDir[0], yDir[0], zDir[0], 0
           , xDir[1], yDir[1], zDir[1], 0
           , xDir[2], yDir[2], zDir[2], 0
           , - dotJSVec(xDir,camera), - dotJSVec(yDir,camera), - dotJSVec(zDir,camera), 1
           ];
}

function perspective(n, f, fovy, aspect) {
    'use strict';
    var h2 = n*Math.tan(fovy/2);
    var w2 = aspect*h2;
    return [ n/w2, 0, 0, 0
           , 0, n/h2, 0, 0
           , 0, 0, (n+f)/(n-f),-1
           , 0, 0, 2*n*f/(n-f), 0 ];
}

function orthogonal(n, f, w, h) {
    'use strict';
    return [ 2/w,  0,          0,  0
           ,  0, 2/h,          0,  0
           ,  0,   0,    2/(n-f),  0
           ,  0,   0, (n+f)/(n-f), 1 ];
}
