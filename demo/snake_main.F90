module snake_main
use screen_handler_m



class(screen), allocatable :: snake_screen


contains


subroutine start()
use game_space_m
use label_m
implicit none
    class(game_space), pointer :: game
    class(label), pointer :: snake_label

    type(vector2) :: bounds

    snake_screen = make_new_screen(20,23)
    call add_screen(snake_screen)
    call set_active_screen(snake_screen)

    allocate(game)
    bounds%x = 20
    bounds%y = 20
    call game%set_size(bounds)
    game%pos%x = 0
    game%pos%y = 3
    game%fill_char = ' '
    game%color_fill%background = 2

    allocate(snake_label)
    bounds%x = 10
    bounds%y = 3
    call snake_label%set_size(bounds)
    snake_label%pos%x = 0
    snake_label%pos%y = 0
    snake_label%color_fill%text = 0
    snake_label%color_fill%background = 7
    call snake_label%set_message("Snake")

    call snake_screen%add_window(game)
    call snake_screen%add_window(snake_label)

end subroutine


end module