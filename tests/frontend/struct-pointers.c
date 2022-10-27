// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 3

struct Point
{
    int x;
    int y;
    int z;
};

int StructAdd(struct Point *P) { return P->x + P->z; }

int test()
{
    struct Point P;
    P.x = 1;
    P.z = 2;
    return StructAdd(&P);
}
