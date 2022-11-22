// RUN: AArch64
// FUNC-DECL: long test(unsigned long long)
// TEST-CASE: test(0) -> 0
// TEST-CASE: test(1) -> 1
// TEST-CASE: test(-1) -> -1

int test(unsigned int c) { return c;}
    