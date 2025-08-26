#include <math.h>

__declspec(dllexport)
void sincos_(double *a, double *s, double *c) {
    // We use the `sin` and `cos` functions from the C standard library (`<math.h>`).
    *s = sin(*a);
    *c = cos(*a);
}
