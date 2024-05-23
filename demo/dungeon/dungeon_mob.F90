module dungeon_mob
use game_space_m
    

    type, extends(object) :: mob

    contains
        procedure :: on_update => mob_update
        procedure :: initialize => mob_initialize
        procedure :: on_destroy => mob_destroy
        procedure :: draw => mob_draw
    end type

contains

    subroutine mob_update(self, input)
    implicit none
        class(mob), target, intent(inout) :: self
        integer :: input
        
    end subroutine

    subroutine mob_initialize(self, parent)
    implicit none
        class(mob), target, intent(inout) :: self
        class(game_space) :: parent

    end subroutine

    subroutine mob_destroy(self)
    implicit none
        class(mob), target, intent(inout) :: self
    end subroutine

    subroutine mob_draw(self, canvas, colors, canvas_bounds)
    implicit none
        class(mob) :: self
        type(color_pair), dimension(:,:), allocatable :: colors
        character, dimension(:,:), allocatable :: canvas
        type(vector2) :: canvas_bounds

    end subroutine



end module