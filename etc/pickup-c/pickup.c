#include <stdio.h>
#include <stdlib.h>
#define MAX_SIZE 15
int next_move(int fibs[], int n, int limit) {
  int current = n;
  int base, k;
  for (int i = 0; i < MAX_SIZE; i++) { /*  update base */
    if (fibs[i] < current) {
      base = fibs[i];
    } else {
      break;
    }
  } /*  done updating base  */
  k = current - base;
  /*  check for 3k <n rule */
  while ((3 * k >= current) || (k > limit)) { /*  reduce game */
    current = k;
    for (int i = 0; i < MAX_SIZE; i++) { /*  update base */
      if (fibs[i] < current) {
        base = fibs[i];
      } else {
        break;
      }
    }
    k = current - base;
  } /*  done reducing game */
  return k;
}
int main() {
  printf("Pick number of sticks (>2) \n");
  int n, k;
  scanf("%d", &n);
  printf("Playing with %d sticks.\n", n);
  int fibs[MAX_SIZE];
  fibs[0] = 1;
  fibs[1] = 2;
  int index = 2;
  while (fibs[index - 1] < n) {
    fibs[index] = fibs[index - 1] + fibs[index - 2];
    if (index == MAX_SIZE) {
      printf("Pick a smaller number to avoid boredom");
      exit(0);
    }
    index++;
  } /*  done populating fibonacci numbers */
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
    if (k > limit) {
      printf("You cannot   pick more than %d.\n", limit);
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
