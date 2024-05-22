module input_handler_m
use curses_m, only: getch


    character :: current_character

contains
    !returns a character and pulls it from the buffer. I need to fix the pulling.
    function get_character() result(out)
    implicit none
        character :: out
        out = current_character
    end function

    !polls the keyboard
    subroutine input_update(input)
    implicit none
        integer :: input
        character :: new_character = char(0)
        current_character = getch()

    end subroutine



end module