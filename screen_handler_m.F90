module screen_handler_m
use screen_m

    type screen_c
        class(screen), pointer :: instance
    contains
    end type

    class(screen_c), dimension(:), allocatable :: screens

    integer :: size=2, index=1
    integer :: active_screen=1

contains

    subroutine register_to_loop(main_loop_o)
        use main_loop_m, only: main_loop
        use action_m, only: action_callback
    implicit none
        class(main_loop) :: main_loop_o

        procedure(action_callback), pointer :: s
        s => refresh_screens

        call main_loop_o%on_update%add_action(s)
    end subroutine


    function make_new_screen(x, y) result(out)
    implicit none
        integer :: x, y
        class(screen), allocatable :: out
        allocate(out)
        allocate(out%canvas(x,y))
        call out%screen_clear()
        out%canvas_size%x = x
        out%canvas_size%y = y        
    end function


    subroutine add_screen(new_screen)
    implicit none
        class(screen), target :: new_screen
        class(screen_c), dimension(:), allocatable :: new_screens
        integer :: i

        if(.not. allocated(screens)) then
            allocate(screens(2))
            screens(1)%instance => new_screen
            return
        end if

        index = index+1
        if(index > size) then
            allocate(new_screens(size*2))
            do i=1, size
                new_screens(i)%instance => screens(i)%instance
            end do
            size = size*2
            new_screens(index)%instance => new_screen
            deallocate(screens)
            screens = new_screens
            return
        end if

        screens(index)%instance => new_screen

    end subroutine

    subroutine remove_screen(screen_to_remove)
    implicit none
        class(screen), target :: screen_to_remove
        integer :: i
        integer :: k

        if(.not. allocated(screens)) then
            return
        end if

        do i=1, index
            if(associated(screens(i)%instance, screen_to_remove)) then
                index = index-1
                do k=i, index
                    screens(k)%instance => screens(k+1)%instance
                end do
                return
            end if
        end do


    end subroutine

    
    subroutine refresh_screens(input)
    use curses_m, only:write_to
    use iso_c_binding
    implicit none
        integer :: input    
        integer(c_int) :: x, y
        call screens(active_screen)%instance%refresh(input)

        do x=1, screens(active_screen)%instance%canvas_size%x
            do y=1, screens(active_screen)%instance%canvas_size%y
                call write_to(x, y, screens(active_screen)%instance%canvas(x,y))
            end do
        end do


    end subroutine

    subroutine set_active_screen(active)
    implicit none
        class(screen), target :: active
        integer :: i

        do i=1, index
            if(associated(screens(i)%instance, active)) then
                active_screen = i
                return
            end if
        end do

    end subroutine

end module