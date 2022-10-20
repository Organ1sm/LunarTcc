// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 3

int callee(int a) { return 1 + a; }

int test() { return callee(2); }
