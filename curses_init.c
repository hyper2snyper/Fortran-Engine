#include <ncurses.h>

void initialize(bool* no_delay)
{
    initscr();
    cbreak();
    nodelay(stdscr, *no_delay);
    raw();
    keypad(stdscr, TRUE);
    noecho();
    refresh();
}

void write_to(int* x, int* y, char* character)
{
    mvaddch(*x, *y, *character);
}

void print(char* str)
{
    printw(str);
}