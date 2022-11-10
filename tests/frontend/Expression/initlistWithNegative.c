// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> -4


int test()
{
    int found_lines_indices[4] = {-1, -2, -3, -4};

    return found_lines_indices[3];
}

void test1() { int a = 1; }
