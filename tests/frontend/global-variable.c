// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 123

int global;

int test()
{
    global = 123;
    return global;
}
