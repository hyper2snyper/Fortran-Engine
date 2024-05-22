!A simple label window.
!It only has one line to display a message


module label_m
use screen_m

    type, extends(window) :: label
        character(len=100), dimension(:), allocatable :: message
        integer :: size = 0
    contains
        procedure :: init
        procedure :: refresh => log_refresh
        procedure :: set_message
    end type

contains
    subroutine log_refresh(self, input)
    implicit none
        class(label) :: self
        integer :: input
        integer :: i, j
        character(len=100) :: str
        
        do i=1, self%size
            do j=1, self%bounds%x-2
                str = self%message(i)
                if(str(j:) == char(0)) then
                    exit
                end if
                self%window_canvas(j+1,i+1) = str(j:)
            end do
        end do

    end subroutine


    subroutine set_message(self, new_message, line)
    implicit none
        class(label) :: self
        character(len=*) :: new_message
        character(len=100) :: formatted_message
        integer, optional :: line
        integer :: p

        p=1
        if(.not. allocated(self%message)) then
            call self%init(1)
        end if

        if(present(line)) then
            p = line
        end if

        formatted_message = trim(new_message)//char(0)
        self%message(p) = formatted_message

    end subroutine

    subroutine init(self, size)
    implicit none
        class(label) :: self
        integer :: size, i
        self%size = size
        allocate(self%message(self%size))
        do i=1, size
            self%message(i) = "" 
        end do
    end subroutine

end module