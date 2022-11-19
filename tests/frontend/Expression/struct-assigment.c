// RUN: AArch64
// FUNC-DECL: unsigned new_tetris()
// TEST-CASE: new_tetris() -> 10

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
    Tetris tetris;
    tetris.curr_piece = (Piece) {.kind = 1, .x = 2, .y = 3, .rotation = 4};

    Tetris tetris1;
    tetris1.curr_piece = (Piece) {.kind = 1, .rotation = 5, .y = 7, .x = 8};

    return tetris.curr_piece.x + tetris1.curr_piece.x;
}
