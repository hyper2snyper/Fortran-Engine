
module curses_m
use, intrinsic :: iso_c_binding

interface
    subroutine init_curses(no_delay) bind(C, name='initialize')
        Import :: c_bool
        implicit none
        logical(c_bool) :: no_delay
    end subroutine
    subroutine curses_refresh() bind(C, name='refresh')
    end subroutine
    subroutine write_to_(x, y, char, text_color, background_color) bind(C, name='write_to')
        Import :: c_int, c_char
        implicit none
        integer(kind=c_int) :: x, y
        character(kind=c_char) :: char
        integer(kind=c_int) :: text_color, background_color
    end subroutine
    function getch() result(char) bind(C, name='getch')
        Import :: c_char
        implicit none
        character(len=c_char) :: char
    end function
end interface




contains


    subroutine write_to(x, y, char, text_color, background_color)
    implicit none
    integer(kind=c_int) :: x, y
    character(kind=c_char) :: char
    integer(kind=c_int), optional :: text_color, background_color
    integer(kind=c_int) :: text_color_ = 7, background_color_ = 0  
    
        if(present(text_color) .and. present(background_color)) then
            text_color_ = text_color
            background_color_ = background_color
        end if

        call write_to_(x,y,char,text_color_,background_color_)
    end subroutine

end module