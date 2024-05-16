module input_handler_m
use curses_m, only: getch

#define BUFFER_SIZE 50

    character, dimension(:) :: buffer(BUFFER_SIZE)
    character :: current_character
    logical :: no_delay = .true.
    integer :: start = 1, end = 1

    

contains

    function get_character() result(out)
    implicit none
        character :: out

        if(.not. no_delay) then
            out = current_character
            return
        end if

        out = char(0)
        if(start == end) then
            return
        end if
       
        out = buffer(start+1)
        start = start+1
        if(start > BUFFER_SIZE) then
            start = 1
        end if
    end function

    subroutine input_update(input)
    implicit none
        integer :: input
        character :: new_character = char(0)
        if(.not. no_delay) then
            current_character = getch()
            return
        end if

        new_character = getch()
        if(new_character == char(0)) then
            return
        end if

        end = end+1
        if(end > BUFFER_SIZE) then
            end = 1
        end if
        buffer(end) = new_character

    end subroutine



end module