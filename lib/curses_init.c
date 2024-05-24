#include <ncurses.h>
#include "colors.c"


void custom_colors()
{
    init_color(BLACK, 0,0,0);
    init_color(WHITE, 1000,1000,1000);
    init_color(RED, 1000, 0, 0);
    init_color(GREEN, 0, 1000, 0);
    init_color(YELLOW, 1000, 1000, 20);
    init_color(BLUE, 0, 0, 1000);
    init_color(MAGENTA, 150, 47, 118);
    init_color(CYAN, 60, 171, 139);
    init_color(GREY, 138, 138, 138);
}

///I stole this
int colornum(int fg, int bg)
{
    int b, f, r, c;
    b = bg << 4;
    r = b | fg;
    //c = 1 << 7;
    
    return r;
}
/// I stole this too, but I know how it works now so ive changed it
void init_colorpairs()
{
    int fg, bg;
    int colorpair;

    for (bg = 0; bg < TOTAL_COLORS; bg++) {
        for (fg = 0; fg < TOTAL_COLORS; fg++) {
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
    custom_colors();
    init_colorpairs();
}



void setcolor(int fg, int bg)
{
    attron(COLOR_PAIR(colornum(fg, bg)) | A_BOLD);
}

void unsetcolor(int fg, int bg)
{
    attroff(COLOR_PAIR(colornum(fg, bg)) | A_BOLD);
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