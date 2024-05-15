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
implicit none
    
    type(main_loop) :: main_i
    procedure(action_callback), pointer :: p => refresh_screens
    class(screen), allocatable :: screen1
    class(game_space), pointer :: log1
    type(vector2) :: bounds
    class(object), pointer :: object1
    procedure(action_callback), pointer :: p1 => rotate_thing

    !main_i%no_delay = .false.

    call main_i%on_update%add_action(p)
    call main_i%on_update%add_action(p1)
    call main_i%initialize()

    screen1 = make_new_screen(100,100)
    call add_screen(screen1)

    allocate(log1)
    
    call screen1%add_window(log1)
    bounds%x = 30
    bounds%y = 30
    call log1%set_size(bounds)
    log1%pos%x = 3
    log1%pos%y = 3

    log1%fill_char = '.'
    log1%color_fill%text = 3
    log1%color_fill%background = 1

    allocate(object1)
    object1%use_multi_sprite = .true.
    object1%multi_sprite%bounds%x = 3
    object1%multi_sprite%bounds%y = 4
    object1%multi_sprite%center%x = 1
    object1%multi_sprite%center%y = 1
    object1%multi_sprite%sprite = "|  ||--||--|"
    object1%multi_sprite%rotation = 0
    object1%color%text = 7
    object1%color%background = 0
    object1%pos%x = 9
    object1%pos%y = 9
    thing => object1
    call log1%add_object(object1)

    call main_i%start_loop()

end program

