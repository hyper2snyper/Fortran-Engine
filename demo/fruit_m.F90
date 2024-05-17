module fruit_m
use game_space_m, only:object, game_space, get_object_at_pos
use object_action_m, only:object_action, object_action_callback
use label_m, only:label


    type, extends(object) :: fruit
    
        type(object_action) :: on_eat
        class(label), pointer :: score_label
        integer :: score = 0
    contains
        procedure :: on_update => fruit_update
        procedure :: initialize => fruit_initialize
        procedure :: on_destroy => fruit_destroy
        procedure :: setup
        procedure :: set_score
    end type

contains
    subroutine setup(self, score, loop)
    implicit none
        class(fruit), target :: self
        class(label), pointer :: score
        type(object_action), intent(inout) :: loop
        procedure(object_action_callback), pointer :: p=>update_tick
        self%score_label => score
        call loop%add_action(self, p)
    end subroutine


    subroutine fruit_update(self, input)
    implicit none
        class(fruit), target, intent(inout) :: self
        integer :: input
    end subroutine

    subroutine fruit_initialize(self, parent)
    implicit none
        class(fruit), target, intent(inout) :: self
        class(game_space) :: parent
        real :: i
        character(len=20) :: score_msg = ""
        self%glyph = '0'
        self%color%background = 1
        self%color%text = 7
        
        call random_number(i)
        self%pos%x = mod(int(i*100), parent%bounds%x-2)
        call random_number(i)
        self%pos%y = mod(int(i*100), parent%bounds%y-2)

        call self%set_score(0)
    end subroutine

    subroutine fruit_destroy(self)
    implicit none
        class(fruit), target, intent(inout) :: self
    end subroutine


    subroutine update_tick(o, input)
    implicit none
        class(object) :: o
        integer :: input
        class(fruit), pointer :: self
        class(object), pointer :: other
        real :: i

        select type(o)
            type is(fruit)
                self => o
        end select

        other => self%parent%get_object_at_pos(self%pos, self)
        if(.not. associated(other)) then
            return
        end if

        call self%set_score(self%score+1)
        call self%on_eat%invoke(0)

        call random_number(i)
        self%pos%x = mod(int(i*100), self%parent%bounds%x-1)+1
        call random_number(i)
        self%pos%y = mod(int(i*100), self%parent%bounds%y-1)+1

    end subroutine

    subroutine set_score(self, new_score)
    implicit none
        class(fruit) :: self
        integer :: new_score
        character(len=20) :: score_msg = ""
        self%score = new_score
        write(score_msg, '(I2)') self%score
        score_msg = "Score:"//score_msg
        call self%score_label%set_message(score_msg)
    end subroutine

end module