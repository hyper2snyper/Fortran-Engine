module log_m
use screen_m

#define LOG_LEN 100
#define LOG_MESSAGE_SIZE 100

    type, extends(window) :: log
        character(len=LOG_MESSAGE_SIZE), dimension(:) :: log_test(LOG_LEN)
        integer :: size
    contains
        procedure :: refresh => log_refresh
        procedure :: add_message
    end type


contains

    subroutine log_refresh(self, input)
    implicit none
        class(log) :: self
        integer :: input

        call self%window_clear()
        call self%draw_border()




    end subroutine

    subroutine add_message(self, text)
    implicit none
        class(log) :: self
        character(len=*) :: text
        character(len=LOG_MESSAGE_SIZE) :: formatted_text

        formatted_text = trim(text)//char(0)


        
    end subroutine


    

end module