module test
use action_m
use game_space_m
implicit none
    
    class(object), pointer :: thing

contains

subroutine rotate_thing(input)
implicit none
    integer :: input
    if(mod(input, 60) /= 0) then
        return
    end if
    thing%multi_sprite%rotation = thing%multi_sprite%rotation+1
    if(thing%multi_sprite%rotation == 4) then
        thing%multi_sprite%rotation = 0
    end if

end subroutine
    
end module test

program main
    use main_loop_m
    use screen_handler_m
    use action_m
    use game_space_m
    use test
    use label_m
implicit none
    
    type(main_loop) :: main_i
    class(screen), allocatable :: screen1
    class(game_space), pointer :: log1
    type(vector2) :: bounds
    class(object), pointer :: object1
    procedure(action_callback), pointer :: p1 => rotate_thing
    class(label), pointer :: label1

    call register_to_loop(main_i)
    call main_i%on_update%add_action(p1)
    call main_i%initialize()

    screen1 = make_new_screen(100,100)
    call add_screen(screen1)

    allocate(log1)
    
    call screen1%add_window(log1)
    bounds%x = 30
    bounds%y = 15
    call log1%set_size(bounds)
    log1%pos%x = 0
    log1%pos%y = 0

    log1%fill_char = '.'
    log1%color_fill%text = 3
    log1%color_fill%background = 1

    allocate(object1)
    object1%use_multi_sprite = .true.
    object1%multi_sprite%bounds%x = 3
    object1%multi_sprite%bounds%y = 4
    object1%multi_sprite%center%x = 2
    object1%multi_sprite%center%y = 2
    object1%multi_sprite%sprite = "| ||-||-|"
    object1%multi_sprite%rotation = 0
    object1%color%text = 7
    object1%color%background = 0
    object1%pos%x = 3
    object1%pos%y = 3
    thing => object1
    call log1%add_object(object1)

    allocate(label1)
    bounds%x=10
    bounds%y=3
    call label1%set_size(bounds)
    label1%pos%x = 10
    label1%pos%y = 1
    label1%color_fill%text = 0
    label1%color_fill%background = 7
    call label1%set_message("test123")
    call screen1%add_window(label1)

    call main_i%start_loop()

end program main

