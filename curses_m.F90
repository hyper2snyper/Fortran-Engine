
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
    subroutine write_to(x, y, char) bind(C, name='write_to')
        Import :: c_int, c_char
        implicit none
        integer(kind=c_int) :: x, y
        character(kind=c_char) :: char
    end subroutine
    function getch() result(char) bind(C, name='getch')
        Import :: c_char
        implicit none
        character(len=c_char) :: char
    end function
end interface




contains

end module