program main
    use main_loop_m
    use screen_handler_m
    use action_m
    use selection_m
implicit none
    
    type(main_loop) :: main_i
    procedure(action_callback), pointer :: p => refresh_screens
    class(screen), allocatable :: screen1
    class(selection), pointer :: selection1
    type(vector2) :: bounds


    !main_i%no_delay = .false.

    call main_i%on_update%add_action(p)
    call main_i%initialize()

    screen1 = make_new_screen(100,100)
    call add_screen(screen1)

    allocate(selection1)
    call selection1%add_selection("option 1")
    call selection1%add_selection("option 2")
    call selection1%add_selection("option 3")
    call selection1%add_selection("option 4")
    
    
    call screen1%add_window(selection1)
    bounds%x = 30
    bounds%y = 30
    call selection1%set_size(bounds)
    selection1%pos%x = 3
    selection1%pos%y = 3

    call main_i%start_loop()

end program

