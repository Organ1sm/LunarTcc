// RUN: AArch64

// FUNC-DECL: int test(int)
// COMPILE-FAIL


int test(int a)
{
    for (int i = ;;)
        return 1;
}
