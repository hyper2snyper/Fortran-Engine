module dungeon_mob
use game_space_m

    type, extends(object) :: mob
        character(len=100) :: name = ""
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
    use dungeon_world, only:world, get_offset
    implicit none
        class(mob) :: self
        type(color_pair), dimension(:,:), allocatable :: colors
        character, dimension(:,:), allocatable :: canvas
        type(vector2) :: canvas_bounds

        class(game_space), pointer :: gs
        class(world), pointer :: pw

        type(vector2) :: offset, actual
        type(color_pair) :: c

        gs => self%parent

        select type(gs)
            type is(world)
                pw => gs
        end select

        offset = pw%get_offset()

        actual = self%pos - offset
        if(actual%x < 1) then
            actual%x = 1
        end if
        if(actual%y < 1) then
            actual%y = 1
        end if
        if(actual%x > pw%bounds%x) then
            actual%x = pw%bounds%x
        end if
        if(actual%y > pw%bounds%y) then
            actual%y = pw%bounds%y
        end if

        canvas(actual%x, actual%y) = self%glyph
        
        if(self%color%text /= -1) then
            colors(actual%x, actual%y)%text = self%color%text
        end if
        if(self%color%background /= -1) then
            colors(actual%x, actual%y)%background = self%color%background
        end if

    end subroutine



end module