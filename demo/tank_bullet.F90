module tank_bullet
use game_space_m


    type, extends(object) :: bullet
        integer :: team
        type(vector2) :: dir
        integer :: speed = 10
    contains
        procedure :: initialize => bullet_init
        procedure :: on_update => bullet_update
        procedure :: on_destroy => bullet_delete
    end type

contains
    subroutine bullet_init(self, parent)
    implicit none
        class(bullet), target, intent(inout) :: self
        class(game_space), pointer :: parent

        self%color%background = self%team
        self%color%text = 7
        self%glyph = "0"
    end subroutine

    subroutine bullet_update(self, input)
    use tank_player, only:player
    use tank_wall, only:wall
    implicit none
        class(bullet), target, intent(inout) :: self
        integer :: input
        class(object), pointer :: other
        class(player), pointer :: p
        class(wall), pointer :: w

        if(mod(input, self%speed) /= 0) then
            return
        end if

        self%pos = self%pos + self%dir

        if(self%pos%x+self%dir%x < 0 .or. self%pos%x > self%parent%bounds%x) then
            call self%delete()
        end if
        if(self%pos%y+self%dir%y < 0 .or. self%pos%y > self%parent%bounds%y) then
            call self%delete()
        end if

        other => self%parent%get_object_at_pos(self%pos+self%dir, self)
        if(.not. associated(other)) then
            return
        end if

        select type(other)
            type is(player)
                p => other
                if(self%team /= p%team) then
                    call p%take_damage(1)
                end if
                call self%delete()
                return
            type is(wall)
                w => other
                if(self%team /= w%team) then
                    call w%take_damage(1)
                end if
                call self%delete()
                return
        end select

    end subroutine

    subroutine bullet_delete(self)
    implicit none
        class(bullet), target, intent(inout) :: self
    end subroutine


end module