!A simple type for storing x and y coordinates


module vector2_m
implicit none

    type, public :: vector2
        integer :: x=0, y=0
    contains
        procedure :: assign
        generic, public :: assignment(=) => assign
        procedure :: add
        generic, public :: operator(+) => add
        procedure :: equals
        generic, public :: operator(==) => equals
    end type

contains

    subroutine assign(self, i)
    implicit none
        class(vector2), intent(inout) :: self
        integer, intent(in) :: i
        self%x = i
        self%y = i
    end subroutine

    function add(self, other) result(out)
    implicit none
        class(vector2), intent(in) :: self
        class(vector2), intent(in) :: other
        type(vector2) :: out 

        out%x = self%x + other%x
        out%y = self%y + other%y
    end function

    function equals(self, other) result(out)
    implicit none
        class(vector2), intent(in) :: self
        class(vector2), intent(in) :: other
        logical :: out
        out = .false.
        if(self%x == other%x .and. self%y == other%y) then
            out = .true.
        end if
    end function


end module