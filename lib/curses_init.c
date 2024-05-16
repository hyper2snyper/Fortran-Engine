#include <ncurses.h>



int colornum(int fg, int bg)
{
    int B, bbb, ffff;

    B = 1 << 7;
    bbb = (7 & bg) << 4;
    ffff = 7 & fg;

    return (B | bbb | ffff);
}


void init_colorpairs()
{
    int fg, bg;
    int colorpair;

    for (bg = 0; bg <= 7; bg++) {
        for (fg = 0; fg <= 7; fg++) {
            colorpair = colornum(fg, bg);
            init_pair(colorpair, fg, bg);
        }
    }
}

void set_no_delay(bool* no_delay)
{
    nodelay(stdscr, *no_delay);
}

void clear_screen()
{
    clear();
}

void initialize()
{
    initscr();
    cbreak();
    raw();
    keypad(stdscr, TRUE);
    noecho();
    start_color();
    init_colorpairs();
}



void setcolor(int fg, int bg)
{
    attron(COLOR_PAIR(colornum(fg, bg)));
}

void unsetcolor(int fg, int bg)
{
    attroff(COLOR_PAIR(colornum(fg, bg)));
}


void write_to(int* x, int* y, char* character, int* text_color, int* background_color)
{

    setcolor(*text_color, *background_color);
    mvaddch(*y, *x, *character);
    unsetcolor(*text_color, *background_color);
    
}

void print(char* str)
{
    printw(str);
}