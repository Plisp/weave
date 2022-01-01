#include <math.h>

#if 0
  #error token()stream\
  \nddd
#else
  #undef ST
#endif

#line 45 "math.h"
#pragma

// beware of double evaluation! we won't support gcc statement expressions
#define max(a,b) \
	((a) > (b) ? (a) : (b))

// decl
static volatile int * const a[3], (*b[2])(int *), *fn(int (*[2])(void));

/* types */
typedef int (*fp2[2])(void);
struct c {
	int slot, slot2;
} *ST;

volatile int *fn1(const struct c (*argc), int (*fpa[2])(void)) {
	char c = '\'', *test = "ߏ" RAWR; // these should be concatenated
	*STRUCT = (struct c){ .slot = 1 };
	(*fp)(to,b);
	(fp2)(to);
	struct {int a[3], b;} w[] = {[0].a = {1}, [1].a[0] = 2};
	register count;
	{
		register n = (count + 7) / 8;
		switch (count % 8) {
		case 0: do { *to = *from++;
		case 1:      *to = *from++;
            } while (--n > 0);
		}
	}
	return 0;
}
