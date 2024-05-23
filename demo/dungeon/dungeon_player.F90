module dungeon_player
use dungeon_mob
use vector2_m
use game_space_m


    type, extends(mob) :: player

    contains
        procedure :: initialize => player_initialize
        procedure :: on_update => player_update
    end type

contains

    subroutine player_initialize(self, parent)
    implicit none
        class(player), target, intent(inout) :: self
        class(game_space) :: parent

        self%glyph = '@'
        self%color%text = 3
        self%color%background = 1

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
        end select

        to_move = w%get_tile_at_pos(moveto)
        if(.not. to_move%passable) then
            return
        end if
        self%pos = moveto
    end subroutine

end module