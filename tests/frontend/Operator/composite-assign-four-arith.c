// RUN: AArch64
// FUNC-DECL: int test(int)
// TEST-CASE: test(0) -> 1
// TEST-CASE: test(1) -> 2

int test(int a)
{
    a += 2;
    a -= 1;
    return a;
}


// FUNC-DECL: int test1(int)
// TEST-CASE: test1(8) -> 8

int test1(int a)
{
    a *= 2;
    a /= 2;

    return a;
}
