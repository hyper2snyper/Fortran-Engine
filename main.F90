program main
    use main_loop_m
    use screen_handler_m
    use action_m
    use log_m
implicit none
    
    type(main_loop) :: main_i
    procedure(action_callback), pointer :: p => refresh_screens
    class(screen), allocatable :: screen1
    class(log), pointer :: log1
    type(vector2) :: bounds


    !main_i%no_delay = .false.

    call main_i%on_update%add_action(p)
    call main_i%initialize()

    screen1 = make_new_screen(100,100)
    call add_screen(screen1)

    allocate(log1)
    call log1%add_message("test1")
    call log1%add_message("test2")
    call log1%add_message("test3")
    call log1%add_message("test4")
    
    call screen1%add_window(log1)
    bounds%x = 30
    bounds%y = 30
    call log1%set_size(bounds)
    log1%pos%x = 3
    log1%pos%y = 3

    call main_i%start_loop()

end program

