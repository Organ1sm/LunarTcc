// RUN: AArch64

// FUNC-DECL: int test()
// TEST-CASE: test() -> 5

int test_void_ptr(void *ptr)
{
    int *p = (int *)ptr;
    return *p;
}

int test()
{
    int a = 5;
    return test_void_ptr(&a);
}

// int test_void_param(void)
// {
//     int a  = 1;
//     int *p = &a;
//
//     void *p1 = (void *)p;
//
//     return test_void_ptr(p1);
// }
