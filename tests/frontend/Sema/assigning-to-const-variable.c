// COMPILE-FAIL


int test(int b)
{
    const int a;
    a = b;
    return a;
}
