
module main_loop_m
use action_m
use curses_m
use input_handler_m, only:input_update, no_delay

    type, public :: main_loop
        real :: seconds_per_update = 1.0/60 !60 fps
        logical :: no_delay = .true.
        type(action) :: on_update
        integer :: cycles = 0
    contains
        procedure :: start_loop
        procedure :: initialize
    end type

contains
    subroutine initialize(self)
    implicit none
        class(main_loop) :: self
        procedure(action_callback), pointer :: p => input_update

        call init_curses(logical(self%no_delay,kind=c_bool))
        call self%on_update%add_action(p)
        no_delay = self%no_delay
    end subroutine

    subroutine start_loop(self)
    implicit none
        class(main_loop) :: self
        real :: current_time
        real :: delta_time
        do while(.true.)
            if(self%no_delay) then
                call cpu_time(delta_time)
                delta_time = delta_time-current_time
                if(delta_time <= self%seconds_per_update) then
                    cycle
                end if
                call cpu_time(current_time)
                self%cycles = self%cycles+1
                call self%on_update%invoke(self%cycles)
                call curses_refresh()
                cycle
            end if
            call self%on_update%invoke(ichar(getch()))
            call curses_refresh()
        end do

    end subroutine

end module