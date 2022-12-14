// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 56

#include "tetris.h"

Tetris new_tetris()
{
    Tetris tetris;

    for (int row = 0; row < BOARD_HEIGHT; row++)
        for (int col = 0; col < BOARD_WIDTH; col++)
            tetris.board[row][col] = 0;

    return tetris;
}

ull board_hole_factor(const Tetris *self)
{
    ull hole_factor = 0;

    for (uint col = 0; col < BOARD_WIDTH; col++)
    {
        uint row = 0;

        // find first non empty block
        while (row < BOARD_HEIGHT && self->board[row][col] == 0)
            row++;

        // if no such block found then continue with the next column
        if (row == BOARD_HEIGHT)
            continue;

        // otherwise count the holes weighted by its depth
        for (uint r = row; r < BOARD_HEIGHT; r++)
            if (self->board[r][col] == 0)
                hole_factor += r;
    }

    return hole_factor;
}

int test()
{
    ull res = 0;
    Tetris tetris;

    tetris = new_tetris();

    tetris.board[18][0] = 1;    // 19
    tetris.board[17][3] = 1;    // 19 + 18

    res += board_hole_factor(&tetris);    // 56

    return res;
}
