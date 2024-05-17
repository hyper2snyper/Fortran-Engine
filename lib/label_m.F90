!A simple label window.
!It only has one line to display a message


module label_m
use screen_m

    type, extends(window) :: label
        character(len=100) :: message = ""
    contains
        procedure :: refresh => log_refresh
        procedure :: set_message
    end type

contains
    subroutine log_refresh(self, input)
    implicit none
        class(label) :: self
        integer :: input
        integer :: i

        do i=1, self%bounds%x-1
            if(self%message(i:) == char(0)) then
                return
            end if
            self%window_canvas(i+1,2) = self%message(i:)
        end do
    end subroutine


    subroutine set_message(self, new_message)
    implicit none
        class(label) :: self
        character(len=*) :: new_message
        character(len=100) :: formatted_message

        formatted_message = trim(new_message)//char(0)
        self%message = formatted_message

    end subroutine

end module