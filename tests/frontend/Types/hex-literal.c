// RUN: AArch64

// FUNC-DECL: unsigned test()
// TEST-CASE: test() -> 15728655

unsigned test()
{
    return 0x00f0000f;
}



// FUNC-DECL: unsigned test_X()
// TEST-CASE: test_X() -> 15728655

// unsigned test_X() { return 0X0f0000f; }
