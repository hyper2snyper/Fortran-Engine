module dungeon_player
use dungeon_mob



    type, extends(mob) :: player

    contains
        procedure :: on_update => player_update
    end type

contains

    subroutine player_update(self, input)
    use input_handler_m
    implicit none
        class(player), target, intent(inout) :: self
        integer :: input
        
        select case(get_character())
            case('w')
                self%pos%y = self%pos%y-1 
            case('s')
                self%pos%y = self%pos%y+1
            case('a')
                self%pos%x = self%pos%x-1
            case('d')
                self%pos%x = self%pos%x+1
        end select



    end subroutine

end module