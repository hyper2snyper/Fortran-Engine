module snake_main
use screen_handler_m
use snake_head, only:player
use fruit_m, only:fruit
use object_action_m, only:object_action, object_action_callback
use timer_m
use game_space_m


class(main_loop), pointer :: main_

class(screen), allocatable :: snake_screen
class(game_space), pointer :: game_s
class(player), pointer :: player_obj
class(fruit), pointer :: fruit_obj
type(object_action) :: snake_loop
integer :: tick_speed = 10
integer :: ticks = 0

logical :: game_ended = .false.


contains

subroutine game_over()
use main_loop_m, only:add_action, action
use label_m
implicit none
    procedure(static_callback), pointer :: p => main_snake_loop
    class(label), pointer :: game_over_label
    type(vector2) :: bounds
    character(len=20) :: msg
    
    game_ended = .true.

    call static_remove_timer(p)
    call game_s%remove_object(player_obj)

    allocate(game_over_label)
    bounds%x = snake_screen%canvas_size%x
    bounds%y = 3
    call game_over_label%set_size(bounds)
    call game_over_label%set_color(0, 7)
    game_over_label%pos%y = snake_screen%canvas_size%y/2
    call snake_screen%add_window(game_over_label)
    write(msg, '(I2)') fruit_obj%score
    msg = "Game Over. Score:"//msg
    call game_over_label%set_message(msg)


end subroutine

subroutine update(input)
use input_handler_m
implicit none
    integer :: input

    if(.not. game_ended) then
        return
    end if

    if(get_character() == char(10)) then
        main_%end = .true.
    end if
end subroutine

subroutine main_snake_loop()
implicit none
    integer :: input
    ticks = ticks+1
    call snake_loop%invoke(ticks)
end subroutine


subroutine start(x, y, main_l)
use label_m
use action_m, only:action_callback
implicit none
    integer :: x, y
    class(main_loop), pointer :: main_l
    class(label), pointer :: snake_label
    class(label), pointer :: score_label
    type(vector2) :: bounds
    procedure(static_callback), pointer :: p => main_snake_loop
    procedure(action_callback), pointer :: ap => update
    procedure(), pointer :: go => game_over

    main_ => main_l

    call random_init(.false., .false.)

    call main_%on_update%add_action(ap)
 
    snake_screen = make_new_screen(x,y+3)
    call add_screen(snake_screen)
    call set_active_screen(snake_screen)

    allocate(game_s)
    bounds%x = x
    bounds%y = y
    call game_s%set_size(bounds)
    game_s%pos%x = 0
    game_s%pos%y = 3
    game_s%fill_char = ' '
    game_s%color_fill%background = 2

    allocate(snake_label)
    bounds%x = x/2
    bounds%y = 3
    call snake_label%set_size(bounds)
    snake_label%pos%x = 0
    snake_label%pos%y = 0
    snake_label%color_fill%text = 0
    snake_label%color_fill%background = 7
    call snake_label%set_message("Snake")

    allocate(score_label)
    bounds%x = x/2
    bounds%y = 3
    call score_label%set_size(bounds)
    score_label%pos%x = x/2
    score_label%pos%y = 0
    score_label%color_fill%text = 0
    score_label%color_fill%background = 7

    call snake_screen%add_window(game_s)
    call snake_screen%add_window(snake_label)
    call snake_screen%add_window(score_label)

    allocate(fruit_obj)
    call fruit_obj%setup(score_label, snake_loop)

    allocate(player_obj)
    call player_obj%setup(fruit_obj, snake_loop, go)

    call game_s%add_object(fruit_obj)
    call game_s%add_object(player_obj)
    

    call static_start_timer(p,tick_speed,.true.)


end subroutine


end module