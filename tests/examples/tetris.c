// RUN: AArch64
// FUNC-DECL: unsigned new_tetris()
// TEST-CASE: new_tetris() -> 16

typedef unsigned uint;

typedef struct piece
{
    uint kind;
    int x;
    int y;
    uint rotation;
} Piece;

typedef struct tetris
{
    unsigned char board[20][10];
    Piece curr_piece;
    uint next_piece_kind;
    uint score;
    Piece bot_target_piece;
} Tetris;

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
