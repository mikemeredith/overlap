#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void densRad(double *x, int *n, double *xpts, int *nxpts,
  double *h, double *result);

static const R_CMethodDef CEntries[] = {
    {"densRad", (DL_FUNC) &densRad, 6},
    {NULL, NULL, 0}
};

void R_init_overlap(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
