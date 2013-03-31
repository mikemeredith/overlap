/* Calculates von Mises kernel density smoother */
/* Mike Meredith mmeredith@wcs.org */
/* Cf. example in "An Introduction to the .C Interface to R" */
/* by Roger D. Peng & Jan de Leeuw */
/* http://www.biostat.jhsph.edu/~rpeng/docs/interface.pdf */

#include <R.h>
#include <Rmath.h>
void densRad(double *x, int *n, double *xpts, int *nxpts,
  double *h, double *result)
{
  int i, j;
  double d, ksum, cons;
  cons = 2 * M_PI * bessel_i(*h, 0, 2);
  for(i=0; i < *nxpts; i++) {
    ksum = 0;
    for(j=0; j < *n; j++) {
      d = xpts[i] - x[j];
      ksum += pow(exp(cos(d) - 1), *h);
    }
    result[i] = ksum / *n / cons;
  }
}

