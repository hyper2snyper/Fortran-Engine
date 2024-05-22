module tanks_main
use main_loop_m, only:main_loop
use screen_m, only:screen
use game_space_m, only:game_space
use vector2_m
use label_m
use tank_player, only:player
use tank_wall, only:wall

    class(main_loop), pointer :: ml
    class(screen), allocatable :: tank_screen
    class(game_space), pointer :: tank_game_space
    class(player), pointer :: player_1
    integer :: team_1 = 1
    class(player), pointer :: player_2
    integer :: team_2 = 4
    logical :: over = .false.

contains

    subroutine game_over(loser)
    implicit none
        integer :: loser
        class(label), pointer :: end_label
        type(vector2) :: bounds
        character(len=20) :: msg = ""
        character(len=10) :: end_msg = " wins!!"
        character(len=30) :: f_msg
        integer :: victor

        over = .true.
        allocate(end_label)
        bounds = tank_game_space%bounds
        bounds%y = 5
        call end_label%set_size(bounds)
        end_label%pos%y = tank_game_space%bounds%y/2
        end_label%color_fill%background = 7
        end_label%color_fill%text = 0

        if(loser == 1) then
            call tank_game_space%remove_object(player_2)
            victor = 2
        else
            call tank_game_space%remove_object(player_1)
            victor = 1
        end if

        write(msg, '(I1)') victor
        msg = "Player "//msg
        f_msg = trim(msg)//end_msg
        call end_label%set_message(f_msg)

        call tank_screen%add_window(end_label)

    end subroutine

    subroutine end_loop(input)
    use input_handler_m
    implicit none
        integer :: input
        if(.not. over) then
            return
        end if

        if(get_character() /= char(10)) then
            return
        end if

        ml%end = .true.
    end subroutine

    subroutine start(x, y, main_l)
    use screen_handler_m
    use action_m, only:action_callback
    implicit none
        integer :: x, y
        class(main_loop), target :: main_l    
        type(vector2) :: bounds
        class(label), pointer :: title
        procedure(action_callback), pointer :: p => end_loop

        ml => main_l
        call ml%on_update%add_action(p)

        tank_screen = make_new_screen(x,y+8)
        allocate(tank_game_space)
        tank_game_space%pos%y = 8
        bounds%x = x
        bounds%y = y
        call tank_game_space%set_size(bounds)
        tank_game_space%fill_char = "."
        call tank_screen%add_window(tank_game_space)

        allocate(title)
        bounds%x = x
        bounds%y = 3
        call title%set_size(bounds)
        title%color_fill%background = 7
        title%color_fill%text = 0
        call title%set_message("Tanks")
        call tank_screen%add_window(title)


        allocate(player_1)
        player_1%pos%x = x/2
        player_1%pos%y = 4
        player_1%multi_sprite%rotation = 2
        call player_1%set_team(team_1)
        call player_1%set_control(1)
        call tank_game_space%add_object(player_1)

        allocate(player_2)
        player_2%pos%x = x/2
        player_2%pos%y = y-3
        call player_2%set_team(team_2)
        call player_2%set_control(2)
        call tank_game_space%add_object(player_2)


        allocate(title)
        bounds%x = x/2
        bounds%y = 5
        call title%set_size(bounds)
        title%pos%y = 3
        call title%init(3)
        call player_1%setup(title, 1)
        call tank_screen%add_window(title)

        allocate(title)
        bounds%x = x/2
        bounds%y = 5
        call title%set_size(bounds)
        title%pos%x = x/2
        title%pos%y = 3
        call title%init(3)
        call player_2%setup(title, 2)
        call tank_screen%add_window(title)

        call make_walls()

        call add_screen(tank_screen)
        call set_active_screen(tank_screen)

    end subroutine


    subroutine make_walls()
    implicit none
        class(wall), pointer :: nwall
        integer :: i

        do i=0, tank_game_space%bounds%x/3
            allocate(nwall)
            call nwall%set_team(0)
            nwall%pos%y = tank_game_space%bounds%y/2
            nwall%pos%x = (i*3)+1
            call tank_game_space%add_object(nwall)
        end do
    end subroutine


end module