// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 123

int global;

int foo()
{
    global = 123;
    return global;
}
