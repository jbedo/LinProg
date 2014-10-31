#include <lp_lib.h>

char
hs_set_row(void *model, int row, int n, int *cols, double *ws)
{
  int i;

  for(i = 0; i < n; i++)
    set_mat(model, row, cols[i], ws[i]);

  return 0;
}
