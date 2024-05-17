!A singleton module for calling actions after a certain amount of time.
!It must be registered to a loop before use.
!Timers can be looping or one-time
!It has support for both object actions and static ones



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
    logical :: pause = .false.
    type(timer_c), dimension(:), allocatable :: timers
    integer :: size=2, index=1

    interface
        subroutine static_callback()
        implicit none
        end subroutine
    end interface
    abstract interface
        subroutine object_callback(self)
        import object
        implicit none
            class(object), intent(inout) :: self
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
        integer :: i, len=0
        type(timer_c), dimension(:), allocatable :: timers_to_remove
        if(pause) then
            return
        end if
        if(index == 0 .or. .not. allocated(timers)) then
            return
        end if
        allocate(timers_to_remove(size))
        do i=1, index
            if(input < timers(i)%start_tick+timers(i)%time) then
                cycle
            end if
            if(timers(i)%is_object) then
                call timers(i)%o_callback(timers(i)%object_to_call)
            else
                call timers(i)%s_callback()
            end if
            if(timers(i)%looping) then                
                timers(i)%start_tick = input
                cycle
            end if
            len = len+1
            timers_to_remove(len) = timers(i)
        end do
        do i=1, len
            call remove_timer(timers_to_remove(i))
        end do
        deallocate(timers_to_remove)

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
        class(object), pointer, intent(in) :: object_to_add
        integer :: delay
        logical :: looping
        type(timer_c) :: new_timer

        new_timer%looping = looping
        new_timer%is_object = .true.
        new_timer%o_callback => callback
        new_timer%object_to_call => object_to_add
        new_timer%time = delay
        new_timer%start_tick = loop%cycles

        call add_timer(new_timer)
    end subroutine 

    subroutine static_remove_timer(callback)
    implicit none
        procedure(static_callback), pointer :: callback
        type(timer_c) :: timer_to_remove

        timer_to_remove%s_callback => callback
        call remove_timer(timer_to_remove)
    end subroutine

    subroutine object_remove_timer(callback, object_to_remove)
    implicit none
        procedure(object_callback), pointer :: callback
        class(object), pointer :: object_to_remove
        type(timer_c) :: timer_to_remove

        timer_to_remove%is_object = .true.
        timer_to_remove%o_callback => callback
        timer_to_remove%object_to_call => object_to_remove
        call remove_timer(timer_to_remove)
    end subroutine

    subroutine add_timer(timer)
    implicit none
        type(timer_c) :: timer
        type(timer_c), dimension(:), allocatable :: new_timers
        integer :: i

        if(.not. allocated(timers)) then
            allocate(timers(size))
            timers(index) = timer
            return
        end if

        index = index+1
        if(index > size) then
            allocate(new_timers(size*2))
            do i=1, size
                new_timers(i) = timers(i)
            end do
            size = size*2
            deallocate(timers)
            timers = new_timers
        end if
        timers(index) = timer

    end subroutine

    subroutine remove_timer(timer)
    implicit none
        type(timer_c) :: timer
        integer i, j
        logical :: found = .false.
        if(.not. allocated(timers) .or. index == 0) then
            return
        end if
        do i=1, index
            if(timers(i)%is_object) then
                if(associated(timers(i)%object_to_call,timer%object_to_call)) then
                    if(associated(timers(i)%o_callback,timer%o_callback)) then
                        found = .true.
                    end if
                end if
            else if(associated(timers(i)%s_callback, timer%s_callback)) then
                found = .true.
            end if
            if(.not. found) then
                cycle
            end if
            index = index-1
            do j=i, index
                timers(j) = timers(j+1)
            end do
            return
        end do
    end subroutine


end module