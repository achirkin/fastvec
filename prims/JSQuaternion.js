function rotScale(quat, vec) {
    'use strict';
    var i = quat[0], j = quat[1], k = quat[2], t = quat[3];
    var x = vec[0], y = vec[1], z = vec[2];
    var l = t*t - i*i - j*j - k*k;
    var d = 2*(i*x + j*y + k*z);
    t *= 2;
    return [ l*x + d*i + t*(z*j - y*k)
           , l*y + d*j + t*(x*k - z*i)
           , l*z + d*k + t*(y*i - x*j)
           ];
}

function getRotScale(a, b) {
    'use strict';
    if (b[0] === 0 && b[1] === 0 && b[2] === 0) { return [0,0,0,0];}
    if (a[0] === 0 && a[1] === 0 && a[2] === 0) { return [Infinity,Infinity,Infinity,Infinity];}
    var ma = Math.hypot(a[0],a[1],a[2]);
    var mb = Math.hypot(b[0],b[1],b[2]);
    var dot = a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
    var c = Math.sqrt(ma*mb + dot);
    ma *= Math.SQRT2;
    return [ (a[1]*b[2] - a[2]*b[1])/ma/c
           , (a[2]*b[0] - a[0]*b[2])/ma/c
           , (a[0]*b[1] - a[1]*b[0])/ma/c
           , c/ma
           ];
}

function axisRotation(axis, a) {
    'use strict';
    if (axis[0] === 0 && axis[1] === 0 && axis[2] === 0) { return [0,0,0,0];}
    var c = Math.cos(a/2), s = Math.sin(a/2)/Math.hypot(axis[0],axis[1],axis[2]);
    return [ axis[0]*s, axis[1]*s, axis[2]*s, c];
}

function fromMatrix3x3(m) {
    'use strict';
    var d = Math.cbrt(
          mat[0]*(mat[4]*mat[8]-mat[5]*mat[7])
        - mat[1]*(mat[3]*mat[8]-mat[5]*mat[6])
        + mat[2]*(mat[3]*mat[7]-mat[4]*mat[6]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[4] - m[8] )) * Math.sign(m[5] - m[7]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] + m[4] - m[8] )) * Math.sign(m[6] - m[2]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] - m[4] + m[8] )) * Math.sign(m[1] - m[3]) * 0.5
           , Math.sqrt(Math.max( 0, d + m[0] + m[4] + m[8] )) * 0.5 ];
}

function fromMatrix4x4(m) {
    'use strict';
    var d = Math.cbrt(
          mat[0]*(mat[5]*mat[10]-mat[6]*mat[9])
        - mat[1]*(mat[4]*mat[10]-mat[6]*mat[8])
        + mat[2]*(mat[4]*mat[ 9]-mat[5]*mat[8]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[5] - m[10] )) * Math.sign(m[6] - m[9]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] + m[5] - m[10] )) * Math.sign(m[8] - m[2]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] - m[5] + m[10] )) * Math.sign(m[1] - m[4]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d + m[0] + m[5] + m[10] )) * 0.5 / m[15] ];
}
