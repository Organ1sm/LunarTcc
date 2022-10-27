// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 18

typedef struct P
{
    int x;
    int y;
} Point;

typedef int i32;

i32 test_add(Point p) { return p.x + p.y; }

i32 test()
{
    Point P;
    P.x = 7;
    P.y = 11;

    return test_add(P);
}
