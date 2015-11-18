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

function qArg(quat) {
    'use strict';
    return Math.atan2( Math.hypot(quat[0],quat[1],quat[2]) , quat[3] ) * 2 ;
}

function getRotScale(a, b) {
    'use strict';
    if (b[0] === 0 && b[1] === 0 && b[2] === 0) { return [0,0,0,0];}
    if (a[0] === 0 && a[1] === 0 && a[2] === 0) { return [Infinity,Infinity,Infinity,Infinity];}
    var t = cross(a, b);
    var ma = Math.hypot(a[0],a[1],a[2]);
    var mb = Math.hypot(b[0],b[1],b[2]);
    var dot = a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
    if (t[0] === 0 && t[1] === 0 && t[2] === 0) {
        if (dot > 0) {return [0,0,0,Math.sqrt(mb/ma)];}
        else         {return [0,0,Math.sqrt(mb/ma),0];}
    }
    var c = Math.sqrt(ma*mb + dot);
    ma *= Math.SQRT2;
    return [ (a[1]*b[2] - a[2]*b[1])/(ma*c)
           , (a[2]*b[0] - a[0]*b[2])/(ma*c)
           , (a[0]*b[1] - a[1]*b[0])/(ma*c)
           , c/ma
           ];
}

function axisRotation(axis, a) {
    'use strict';
    if (axis[0] === 0 && axis[1] === 0 && axis[2] === 0) { return [0,0,0,1];}
    var c = Math.cos(a*0.5), s = Math.sin(a*0.5) / Math.hypot(axis[0],axis[1],axis[2]);
    return [ axis[0]*s, axis[1]*s, axis[2]*s, c];
}

function fromMatrix3x3(m) {
    'use strict';
    var d = Math.cbrt(
          m[0]*(m[4]*m[8]-m[5]*m[7])
        - m[1]*(m[3]*m[8]-m[5]*m[6])
        + m[2]*(m[3]*m[7]-m[4]*m[6]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[4] - m[8] )) * Math.sign(m[5] - m[7]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] + m[4] - m[8] )) * Math.sign(m[6] - m[2]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] - m[4] + m[8] )) * Math.sign(m[1] - m[3]) * 0.5
           , Math.sqrt(Math.max( 0, d + m[0] + m[4] + m[8] )) * 0.5 ];
}

function fromMatrix4x4(m) {
    'use strict';
    var d = Math.cbrt(
          m[0]*(m[5]*m[10]-m[6]*m[9])
        - m[1]*(m[4]*m[10]-m[6]*m[8])
        + m[2]*(m[4]*m[ 9]-m[5]*m[8]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[5] - m[10] )) * Math.sign(m[6] - m[9]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] + m[5] - m[10] )) * Math.sign(m[8] - m[2]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] - m[5] + m[10] )) * Math.sign(m[1] - m[4]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d + m[0] + m[5] + m[10] )) * 0.5 / m[15] ];
}
