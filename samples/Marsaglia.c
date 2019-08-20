#include <stdio.h>
#define znew   (z=36969*(z&65535)+(z>>16))
#define wnew   (w=18000*(w&65535)+(w>>16))
#define MWC    ((znew<<16)+wnew )
#define SHR3  (jsr^=(jsr<<17), jsr^=(jsr>>13), jsr^=(jsr<<5))
#define CONG  (jcong=69069*jcong+1234567)
#define FIB   ((b=a+b),(a=b-a))
#define KISS  ((MWC^CONG)+SHR3)
#define LFIB4 (c++,t[c]=t[c]+t[UC(c+58)]+t[UC(c+119)]+t[UC(c+178)])
#define SWB   (c++,bro=(x<y),t[c]=(x=t[UC(c+34)])-(y=t[UC(c+19)]+bro))
#define UNI   (KISS*2.328306e-10)
#define VNI   ((long) KISS)*4.656613e-10
#define UC    (unsigned char)  /*a cast operation*/
typedef unsigned long UL;

/*  Global static variables: */
 static UL z=362436069, w=521288629, jsr=123456789, jcong=380116160;
 static UL a=224466889, b=7584631, t[256];
/* Use random seeds to reset z,w,jsr,jcong,a,b, and the table t[256]*/

 static UL x=0,y=0,bro; static unsigned char c=0;

/* Example procedure to set the table, using KISS: */
void settable(UL i1,UL i2,UL i3,UL i4,UL i5, UL i6)
{ int i; z=i1;w=i2,jsr=i3; jcong=i4; a=i5; b=i6;
for(i=0;i<256;i=i+1)  t[i]=KISS;
}

/* This is a test main program.  It should compile and print 7  0's. */
int main(void){
int i; UL k;
settable(12345,65435,34221,12345,9983651,95746118);
for(i=1;i<1000001;i++){k=LFIB4;} printf("%u\n", k-1064612766U);
for(i=1;i<1000001;i++){k=SWB  ;} printf("%u\n", k- 627749721U);
for(i=1;i<1000001;i++){k=KISS ;} printf("%u\n", k-1372460312U);
for(i=1;i<1000001;i++){k=CONG ;} printf("%u\n", k-1529210297U);
for(i=1;i<1000001;i++){k=SHR3 ;} printf("%u\n", k-2642725982U);
for(i=1;i<1000001;i++){k=MWC  ;} printf("%u\n", k- 904977562U);
for(i=1;i<1000001;i++){k=FIB  ;} printf("%u\n", k-3519793928U);
}
