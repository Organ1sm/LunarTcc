int g;

int foo(int a, int b) { return g = a - b; }

int bar() {
  int i;
  double real;
  int arr[10];


  bar();

  real = 0.0;
  if (g == 0)
    real = foo(1, 2.0);
  else
    g = 0;

  i = 10;
  while (i > 0 && real != 0.0) {
    arr[i] = real = 3.0 * (i % 20);
    i = i + 1;
  }

  return arr[5] & 3;
}
