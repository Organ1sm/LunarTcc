// RUN: AArch64

// FUNC-DECL: int test(int)
// COMPILE-FAIL


int test(int b)
{
    const int a;
    a = b;
    return a;
}