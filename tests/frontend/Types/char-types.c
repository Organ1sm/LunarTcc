// RUN: AArch64
// FUNC-DECL: char test()
// FUNC-DECL: unsigned char test1()
// TEST-CASE: test() -> 97
// TEST-CASE: test1() -> 98

char test() { return 'a'; }

unsigned char test1() { return 98; }