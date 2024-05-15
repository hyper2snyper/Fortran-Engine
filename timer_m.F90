module timer_m
use main_loop_m
use game_space_m, only: object
    
    type :: timer_c
        procedure(static_callback), pointer, nopass :: s_callback => null()
        procedure(object_callback), pointer, nopass :: o_callback => null()
        class(object), pointer :: object_to_call => null()
        logical :: is_object = .false.
        logical :: looping = .false.
        integer :: start_tick = 0
        integer :: time
    end type

    class(main_loop), pointer :: loop
    type(timer_c), dimension(:), allocatable :: timers
    integer :: size=2, index=1

    interface
        subroutine static_callback()
        implicit none
        end subroutine
        subroutine object_callback(self)
        import object
        implicit none
            class(object) :: self
        end subroutine
    end interface

contains
    subroutine register(loop_)
    implicit none
        class(main_loop), pointer :: loop_
        procedure(action_callback), pointer :: p => update_tick
        loop => loop_
        call loop%on_update%add_action(p)
    end subroutine

    subroutine update_tick(input)
    implicit none
        integer :: input
    end subroutine

    subroutine static_start_timer(callback, delay, looping)
    implicit none
        procedure(static_callback), pointer :: callback
        integer :: delay
        logical :: looping
        type(timer_c) :: new_timer

        new_timer%looping = looping
        new_timer%s_callback => callback
        new_timer%time = delay
        new_timer%start_tick = loop%cycles

        call add_timer(new_timer)
    end subroutine

    subroutine object_start_timer(callback, object_to_add, delay, looping)
    implicit none
        procedure(object_callback), pointer :: callback
        class(object), pointer :: object_to_add
        integer :: delay
        logical :: looping
    end subroutine 

    subroutine static_remove_timer(callback)
    implicit none
        procedure(static_callback), pointer :: callback
    end subroutine

    subroutine object_remove_timer(callback, object_to_remove)
    implicit none
        procedure(object_callback), pointer :: callback
        class(object), pointer :: object_to_remove
    end subroutine

    subroutine add_timer(timer)
    implicit none
        type(timer_c) :: timer
    end subroutine

    subroutine remove_timer(timer)
    implicit none
        type(timer_c) :: timer
    end subroutine


end module