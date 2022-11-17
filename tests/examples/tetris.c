// RUN: AArch64
// FUNC-DECL: unsigned new_tetris()
// TEST-CASE: new_tetris() -> 16

typedef unsigned uint;

uint new_tetris()
{
    unsigned char board[20][10];
    int row;
    int col;

    for (row = 0; row < 20; row++)
        for (col = 0; col < 10; col++)
            board[row][col] = row * col;

    return board[4][4];
}
