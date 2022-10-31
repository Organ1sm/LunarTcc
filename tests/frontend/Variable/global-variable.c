// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 123

int g2;

int test()
{
    g2 = 123;
    return g2;
}
