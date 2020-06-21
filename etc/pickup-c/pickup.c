#include <stdio.h>
#include <stdlib.h>
#define MAX_SIZE 16

int fib_base(const int fibs[], const int current) {
  int base = 0;
  for (int i = 0; i < MAX_SIZE; i++) { /*  update base */
    if (fibs[i] < current) {
      base = fibs[i];
    } else {
      break;
    }
  } /*  done updating base  */
  return base;
}

int next_move(const int fibs[], int n, int limit) {
  int current = n;
  int k;
  int base = fib_base(fibs, current);
  k = current - base;
  /*  check for 3k <n rule */
  while ((3 * k >= current) || (k > limit)) { /*  reduce game */
    current = k;
    base = fib_base(fibs, current);
    k = current - base;
  } /*  done reducing game */
  return k;
}

int main() {
  int fibs[MAX_SIZE];
  fibs[0] = 1;
  fibs[1] = 2;
  for (int index = 2; index < MAX_SIZE; index++) {
    fibs[index] = fibs[index - 1] + fibs[index - 2];
  } /*  done populating fibonacci numbers */
  int k;
  int n;
  printf("Pick number of sticks between 2 and 2000.\n");
  scanf("%d", &n);
  if ((n < 2) || (2000 < n)) {
    printf("Pick  a number between 2 and 2000.\n", fibs[MAX_SIZE - 1]);
    exit(0);
  }

  printf("Playing with %d sticks.\n", n);
  int limit = n - 1;
  int fib_p = 0;
  for (int i = 0; i < MAX_SIZE; i++) {
    if (fibs[i] == n) {
      fib_p = 1;
      break;
    }
  }                 /*  fib_p */
  if (fib_p != 1) { /*  first move */
    printf("I play first.\n");
    k = next_move(fibs, n, limit);
    n -= k;
    limit = 2 * k;
    printf("I pick %d. %d  left; You can pick up to %d.\n", k, n, limit);
  }               /*  first turn */
  while (n > 0) { /*  take turns */
    printf("How many sticks do you pick?\n");
    scanf("%d", &k);
    if (k > limit || k < 0) {
      printf("You can only pick between 1 and %d.\n", limit);
      continue;
    }
    n -= k;
    limit = 2 * k;
    printf("You picked %d, %d left.\n", k, n);
    if (limit >= n) {
      printf("I pick %d and win!\n", n);
      exit(0);
    }
    k = next_move(fibs, n, limit);
    n -= k;
    limit = 2 * k;
    if (n == 0) {
      printf("I picked %d and won!", k);
      exit(0);
    } else {
      printf("I pick %d. %d  left; You can pick up to %d.\n", k, n, limit);
    }
  }
}
