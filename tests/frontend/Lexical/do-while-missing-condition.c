// RUN: AArch64

// FUNC-DECL: int test(int)
// COMPILE-FAIL


int test(int a)
{
    do
    {
        return a;
    }
    while ();
}