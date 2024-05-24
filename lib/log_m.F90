!A log window that can display scrolling messages


module log_m
use screen_m

#define LOG_LEN 100
#define LOG_MESSAGE_SIZE 100

    type :: log_message
        character(len=LOG_MESSAGE_SIZE) :: message = ""

    end type


    type, extends(window) :: log
        type(log_message), dimension(:) :: log_list(LOG_LEN)
        integer :: size = 0
    contains
        procedure :: refresh => log_refresh
        procedure :: add_message
    end type


contains

    subroutine log_refresh(self, input)
    implicit none
        class(log) :: self
        integer :: input
        integer :: i
        integer :: j
        integer :: y

        if(self%size == 0) then
            return
        end if

        do i=1, self%size
            do j=1, self%bounds%x-2
                if(self%log_list(i)%message(j:) == char(0)) then
                    exit
                end if
                y = self%bounds%y-(self%size-i)-1
                if(y < 2) then
                    exit
                end if
                self%window_canvas(j+1, y) = self%log_list(i)%message(j:)
            end do
        
        end do


    end subroutine

    subroutine add_message(self, text)
    implicit none
        class(log) :: self
        character(len=*) :: text
        character(len=LOG_MESSAGE_SIZE) :: formatted_text
        integer :: i

        formatted_text = trim(text)//char(0)
        self%size = self%size+1

        if(self%size > LOG_LEN) then
            do i=1, LOG_LEN-1
                self%log_list(i)%message = self%log_list(i+1)%message
            end do   
            self%size = LOG_LEN         
        end if

        self%log_list(self%size)%message = formatted_text
       
        
    end subroutine

    subroutine log_clear(self)
    implicit none
        class(log) :: self

        self%size = 0
    end subroutine
    

end module