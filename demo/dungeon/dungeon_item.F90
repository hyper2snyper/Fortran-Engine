module dungeon_item
use dungeon_mob, only: mob
use vector2_m;


    type, extends(mob) :: item
        
    contains
        procedure :: pickup
        procedure :: set_down
    end type

contains
    subroutine pickup(self, picker)
    implicit none
        class(item) :: self
        class(mob) :: picker

    end subroutine

    subroutine set_down(self, pos)
    implicit none
        class(item) :: self
        type(vector2) :: pos

    end subroutine


end module