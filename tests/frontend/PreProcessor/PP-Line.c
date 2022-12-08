// RUN: AArch64

// FUNC-DECL: int test(int)
// TEST-CASE: test(0) -> 13
// TEST-CASE: test(1) -> 15
// TEST-CASE: test(123) -> 10

int test(int a)
{
    unsigned res = __LINE__;    // 10

    if (a == 0)
        res = __LINE__;    // 13
    else if (a == 1)
        res = __LINE__;    // 15

    return res;
}
