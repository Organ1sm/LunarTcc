// RUN: AArch64
// FUNC-DECL: unsigned new_tetris()
// TEST-CASE: new_tetris() -> 16


typedef unsigned uint;

#define HEIGHT 20
#define WIDTH  10

uint new_tetris()
{
    unsigned char board[20][10];
    int row;
    int col;

    for (row = 0; row < HEIGHT; row++)
        for (col = 0; col < WIDTH; col++)
            board[row][col] = row * col;

    return board[4][4];
}
