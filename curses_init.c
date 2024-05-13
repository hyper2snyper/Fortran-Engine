#include <ncurses.h>

void initialize(bool* no_delay)
{
    initscr();
    cbreak();
    nodelay(stdscr, *no_delay);
    raw();
    keypad(stdscr, TRUE);
    noecho();
    start_color();
    refresh();
}

void write_to(int* x, int* y, char* character, int* text_color, int* background_color)
{
    
    mvaddch(*x, *y, *character);
}

void print(char* str)
{
    printw(str);
}