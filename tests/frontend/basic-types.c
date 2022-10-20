// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test('a' + 1) -> 98

char test(char a, int b) { return a + b; }
