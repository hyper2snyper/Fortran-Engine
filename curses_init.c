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

    init_pair(1, *text_color,*background_color);
    //attron(COLOR_PAIR(1));

    mvaddch(*x, *y, *character | COLOR_PAIR(1));
    //attroff(COLOR_PAIR(1));
    
}

void print(char* str)
{
    printw(str);
}