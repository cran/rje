#include <stdio.h>
#include <math.h>

void indexBox (int *upp, int *lwr, int *dim, int *len, int *out) {
  int i, j, k;
  int prod = 1;
  int totlen = 1;
  out[0] = 0;
  int offset = 0;

  for (i = 0; i < *len; i++) {
    offset += prod*lwr[i];
    for (j = 0; j < upp[i] - lwr[i] + 1; j++) {
      for (k = 0; k < totlen; k++) {
	out[k + totlen*j] = out[k] + prod*j;
      }
    }
    prod *= dim[i];
    totlen *= (upp[i] - lwr[i] + 1);
  }
  for (k = 0; k < totlen; k++) out[k] += offset;
}


void arrayInd (int *ind, int *dim, int *nind, int *ndim, int *out) {
  int i,j;
  int rem;
  int prod = 1;

  for (j = 0; j < *ndim; j++) prod *= dim[j];

  for (i = 0; i < *nind; i++) {
    if (ind[i] < 1 || ind[i] > prod)

    ind[i] -= 1;

    for (j = 0; j < *ndim; j++) {
      rem = ind[i] % dim[j];
      out[i + j*nind[0]] = rem + 1;
      ind[i] -= rem;
      ind[i] /= dim[j];
    }
  }
}

