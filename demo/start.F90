module start
use main_loop_m
use screen_handler_m
use screen_m, only:screen
use selection_m, only:selection, selection_action
use timer_m
implicit none

class(main_loop), pointer :: main_l

contains

subroutine run()
implicit none
    class(screen), allocatable :: main_screen
    class(selection), allocatable :: main_selection
    type(vector2) :: bounds
    procedure(selection_action), pointer :: p

    allocate(main_l)

    call main_l%set_no_delay(.true.)
    call main_l%initialize()

    
    call register_to_loop(main_l)
    main_screen = make_new_screen(50, 25)

    allocate(main_selection)
    bounds%x = 10
    bounds%y = 10
    main_selection%pos%x = 20
    main_selection%pos%y = 10
    call main_selection%set_size(bounds)
    p => start_snake
    call main_selection%add_selection("Snake", p)
    p => quit
    call main_selection%add_selection("Quit", p)

    call main_screen%add_window(main_selection)

    call add_screen(main_screen)


    call register(main_l)

    call main_l%start_loop()

    call endwin()

end subroutine

subroutine quit(selection_box, index)
implicit none
    class(selection) :: selection_box
    integer :: index

    main_l%end = .true.
end subroutine

subroutine start_snake(selection_box, index)
use snake_main, only:start
implicit none
    class(selection) :: selection_box
    integer :: index

    call start(30, 25, main_l)
end subroutine



end module