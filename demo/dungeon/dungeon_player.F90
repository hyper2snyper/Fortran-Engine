module dungeon_player
use dungeon_mob
use vector2_m
use game_space_m
use log_m
use label_m

    type, extends(mob) :: player
        class(log), pointer :: player_log
        class(label), pointer :: player_status
    contains
        procedure :: initialize => player_initialize
        procedure :: on_update => player_update
    end type

contains

    subroutine player_initialize(self, parent)
    implicit none
        class(player), target, intent(inout) :: self
        class(game_space) :: parent

        type(vector2) :: bounds

        self%glyph = '@'
        self%color%text = 3
        self%color%background = 1

        allocate(self%player_log)
        bounds%x = 30
        bounds%y = self%parent%parent%canvas_size%y-10
        call self%player_log%set_size(bounds)
        self%player_log%pos%x = self%parent%parent%canvas_size%x-30
        self%player_log%pos%y = 10
        call self%parent%parent%add_window(self%player_log)
        call self%player_log%add_message("Welcome Adventurer")

        allocate(self%player_status)
        bounds%x = 30
        bounds%y = 10
        call self%player_status%set_size(bounds)
        self%player_status%pos%x = self%parent%parent%canvas_size%x-30
        call self%parent%parent%add_window(self%player_status)
        call self%player_status%init(8)
        call self%player_status%set_message("Status:", 1)


    end subroutine

    subroutine player_update(self, input)
    use input_handler_m
    use dungeon_world, only:get_tile_at_pos, tile, world
    implicit none
        class(player), target, intent(inout) :: self
        integer :: input
        
        type(vector2) :: moveto
        type(tile) :: to_move
        class(game_space), pointer :: o
        class(world), pointer :: w

        o => self%parent

        select type(o)
            type is(world)
                w => o
        end select

        moveto = self%pos
        select case(get_character())
            case('w')
                moveto%y = self%pos%y-1 
            case('s')
                moveto%y = self%pos%y+1
            case('a')
                moveto%x = self%pos%x-1
            case('d')
                moveto%x = self%pos%x+1
            case(char(255))
                return
        end select

        if(self%pos%x < 1 .or. self%pos%y < 1) then
            return
        end if
        if(self%pos%x > w%levels(w%current_level)%size%x .or. self%pos%y > w%levels(w%current_level)%size%y) then
            return
        end if

        to_move = w%get_tile_at_pos(moveto)
        if(.not. to_move%passable) then
            return
        end if
        self%pos = moveto
        call self%player_log%add_message("Adventurer moves.")
    end subroutine

end module