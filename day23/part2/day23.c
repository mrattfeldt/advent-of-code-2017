#include <stdio.h>

int main() {
  int b, c, d, f, h;

  h = 0;

  b = 109900;
  c = 126900;

  while (1) {
    f = 1;
    d = 2;

    do {

      if (b % d == 0) f = 0;

      d++;

    } while (d != b);

    if (f == 0) h++;

    if (b == c) {
      printf("%d\n", h);
      return 0;
    }

    b += 17;
  }
}
