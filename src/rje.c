#include <R.h>
#include <Rinternals.h>
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

void doone (double *x, int *dim, int k, int rmv) {
  int i1, i2, i3;
  int before = 1, before2, after = 1;
  long double y = 0.0;

  for (i1=0; i1 < rmv-1; i1++) {
    before *= dim[i1];
  }
  before2 = before*dim[rmv-1];
  for (i1=rmv; i1 < k; i1++) {
    after *= dim[i1];
  }

  for (i3 = 0; i3 < after; i3++) {
    for (i1 = 0; i1 < before; i1++) {
      y = 0.0;
      for (i2 = 0; i2 < dim[rmv-1]; i2++) {
	y += x[i1+before*i2+before2*i3];
      }
      x[i1+before*i3] = y;
    }
  }

  return;
}

void marginTable (double *x, int *dim, int *k, int *rmv, int *nrmv) {
  for (int i=0; i < nrmv[0]; i++) {
    if (dim[rmv[i]-1] > 1) doone(x, dim, k[0], rmv[i]);

    k[0] -= 1;
    for (int j=rmv[i]-1; j < k[0]; j++) {
      dim[j] = dim[j+1];
    }
    for (int j=i+1; j < nrmv[0]; j++) {
      rmv[j] -= (rmv[j] > rmv[i] ? 1 : 0);
    }
  }
}

