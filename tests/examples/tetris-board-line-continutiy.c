// RUN: AArch64
// FUNC-DECL: int test()
// TEST-CASE: test() -> 86

#include "tetris.h"

Tetris new_tetris()
{
    Tetris tetris;

    for (int row = 0; row < BOARD_HEIGHT; row++)
        for (int col = 0; col < BOARD_WIDTH; col++)
            tetris.board[row][col] = 0;

    return tetris;
}

ull board_line_continuity(const Tetris *self)
{
    ull continuity_factor = 0;

    for (int row = BOARD_HEIGHT - 1; row >= 0; row--)
    {
        ull curr_line_con_from_left  = 0;
        ull curr_line_con_from_right = 0;
        uint col                     = 0;

        while (col < BOARD_WIDTH && self->board[row][col++] != 0)
            curr_line_con_from_left++;

        col = BOARD_WIDTH;
        while (col > 0 && self->board[row][col-- - 1] != 0)
            curr_line_con_from_right++;

        const uint factor = MAX(curr_line_con_from_left, curr_line_con_from_right);
        continuity_factor += factor * factor * row;
    }

    return continuity_factor;
}

int test()
{
    ull res = 0;
    Tetris tetris;

    tetris = new_tetris();

    tetris.board[10][9] = 1;    // 10

    // 76
    tetris.board[19][0] = 1;
    tetris.board[19][1] = 1;

    res += board_line_continuity(&tetris);

    return res;
}
