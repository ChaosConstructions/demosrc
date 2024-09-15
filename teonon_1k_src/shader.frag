//
//  Teonon 1k generated image by dart/fenomen for CC`2024
//

#version 330

uniform vec2 iResolution = vec2(1920,1080);
//uniform float t;
//float iTime = t;

void mainImage(out vec4 fragColor, in vec2 fragCoord);
void main(void) { mainImage(gl_FragColor, vec2(gl_FragCoord.xy) ) ; }
////////////////////////////////////////////////

vec3 curColor, p0;
int i;
//float pi = 3.1415;
//float dbpi = 6.28318;

float hash12(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * .3333);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

float sdBox(vec3 p, vec3 s) {
    p = abs(p) - s;
    return max(max(p.x,p.y),p.z);
}

mat2 rot(float a) {
    return mat2(cos(a), sin(a), -sin(a), cos(a));
}


float map(vec3 p)
{
    float d = 1e3;
    
    //d = min(boxField2(p, d, 1.), d);
    {
        p0 = p + vec3(0,1,.1);
        p0.xy *= rot(1.);
        for (int i = 0; i < 20; ++ i) {    
            p0.xz *= rot(0.314);
            vec3 p01 = p0 + vec3(3,0,1);
            p01.yx *= rot(1.);
            d = min(d, sdBox(p01, vec3(0.04,9,0.2)));
        }
    }
    
    //p = abs(pos) - 4.;
//    vec3 p1 = mod(pos, 12) - 9 ;
//        d = min2(d, sdBox(p1, vec3(2)), 2);
//    d = min2(d, sdBox(p1 + 1.4, vec3(1)), 2);
//    d = min2(d, sdBox(p1 + vec3(2.4,5,0), vec3(0.5, 3, 0.4)), 2);

    //p.y -= 4;
    
    //d = min2(d, max(length(p.xz) - k, p.y), 2);
    //d = min2(d, max(length(p.yz) - k, p.x), 2);
    //d = min2(d, max(length(p.xy) - k, p.z), 2);

    //d = min(d, length(p.yz - (sin(p.x * 0.9) * .9)));

    //d = min2(d, p.y+11 + sin(p.z*0.2)*0.5, 3);
    //d = min2(d, p.y+5.8 + sin(p.x*0.72)*0., 3);

    
    //d = min(d, -sdBox(p, vec3(12)));
    d = min(d, -length(p) + 7);
    p0 = p + vec3(1,.6,4);
    for (i = 0; i<35;++i) {
    p0 += vec3(0.23,0.3,1);
    p0.xz *= rot(-.7);
    p0.zy *= rot(-1.7);
    d = min(d, sdBox(p0, vec3(0.05)));
    }
    //d = min(d, length(p) - 1);
    //d = max(d, -length(p) - 8);
    //d = min(d, sdBox(p, vec3(1)));

    // threads
    //p.xz *= rot(2.2);
    //p.x -= 7.3;

    // busi
    //p.y = mod(p.y, 2.4) - 1.4;

    
    //d = min(d, length(p.xz) - .2);
    curColor = abs(p - vec3(0,0,3))*3;
    return d;
}

vec3 normal(vec3 p) {
    vec3 e = vec3(0,.01,1.);
    return normalize(vec3(map(p + e.yxx), map(p + e.xyx), map(p + e.xxy)) - map(p));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord / iResolution.xy - .5;
    uv.x *= iResolution.x / iResolution.y;
    vec3 from = vec3(0, -.0, -6);
    vec3 p = from, col;
    vec3 dir = normalize(vec3(uv, 1));
    float d;
    float td;
    for (i = 0; i < 100; i++ ) {
        d = map(p) * (1 - hash12(fragCoord.xy) * .1);
        if ( d < 0 ) {
            dir = reflect(dir, normal(p));
            d = .1;
        }
        col += curColor * .001;
        d = max(d, .001);
        p += d * dir;
        td += d;
        if ( td > 30.)
            break;
    }
    fragColor = vec4(pow(col, vec3(0.95)), 1);
}