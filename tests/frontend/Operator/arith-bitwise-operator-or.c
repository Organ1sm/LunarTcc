// RUN: AArch64

// FUNC-DECL: int test_or(unsigned)
// TEST-CASE: test_or(1) -> 15
// TEST-CASE: test_or(255) -> 255
// TEST-CASE: test_or(7) -> 15


unsigned test_or(unsigned a) { return a | 15; }


