module snake_tail_m
use game_space_m, only: object, game_space
use object_action_m, only:object_action_callback, object_action

    type, extends(object) :: snake_tail
        class(snake_tail), pointer :: head => null()
        procedure(), pointer, nopass :: game_over
    contains
        procedure :: on_update => tail_update
        procedure :: initialize => tail_initialize
        procedure :: on_destroy => tail_destroy
        procedure :: assign_to_loop
    end type

contains

    subroutine assign_to_loop(self, loop)
    implicit none
        class(snake_tail), target :: self
        type(object_action), intent(inout) :: loop

        procedure(object_action_callback), pointer :: p
        p => tail_tick_update

        call loop%add_action(self, p)
    end subroutine


    subroutine tail_update(self, input)
    use game_space_m, only:get_object_at_pos
    implicit none
        class(snake_tail), target, intent(inout) :: self
        integer :: input
        class(object), pointer :: o => null()

        o => self%parent%get_object_at_pos(self%pos, self)
        if(.not. associated(o)) then
            return
        end if
        if(same_type_as(o, self)) then
            return
        end if
        call self%game_over()
    end subroutine
    
    subroutine tail_initialize(self, parent)
    implicit none
        class(snake_tail), target, intent(inout) :: self
        class(game_space) :: parent
        self%glyph = '.'
        self%color%text = 7

    end subroutine

    subroutine tail_destroy(self)
    implicit none
        class(snake_tail), target, intent(inout) :: self
    end subroutine


    subroutine tail_tick_update(o, tick)
    implicit none
        class(object), pointer :: o
        integer :: tick
        class(snake_tail), pointer :: self
        select type(o)
            type is(snake_tail)
                self => o
        end select

    

    end subroutine

end module