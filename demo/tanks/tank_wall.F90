module tank_wall
use game_space_m

    type, extends(object) :: wall
        integer :: team
        integer :: health = 3
        character(len=3) :: health_glyphs = "/H%"
    contains
        procedure :: initialize => wall_init
        procedure :: on_update => wall_update
        procedure :: on_destroy => wall_delete
        procedure :: set_team
        procedure :: take_damage
    end type


contains

    subroutine take_damage(self, damage)
    implicit none
        class(wall) :: self
        integer :: damage, i
        self%health = self%health - damage
        i = self%health
        self%multi_sprite%sprite(1:3) = self%health_glyphs(i:i)//self%health_glyphs(i:i)//self%health_glyphs(i:i)
        if(self%health <= 0) then
            call self%delete()
        end if
    end subroutine

    subroutine set_team(self, team)
    implicit none
        class(wall) :: self
        integer :: team
        self%team = team
        self%color%text = team
    end subroutine

    subroutine wall_init(self, parent)
    implicit none
        class(wall), target, intent(inout) :: self
        class(game_space), pointer :: parent

        self%use_multi_sprite = .true.
        self%multi_sprite%sprite(1:3) = self%health_glyphs(3:3)//self%health_glyphs(3:3)//self%health_glyphs(3:3)
        self%multi_sprite%bounds%x = 3
        self%multi_sprite%bounds%y = 1
        self%multi_sprite%center%x = 2
        self%multi_sprite%center%y = 1

        self%color%background = 7

    end subroutine

    subroutine wall_update(self, input)
    implicit none
        class(wall), target, intent(inout) :: self
        integer :: input
    end subroutine

    subroutine wall_delete(self)
    implicit none
        class(wall), target, intent(inout) :: self
    end subroutine


end module