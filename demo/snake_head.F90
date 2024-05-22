module snake_head
use game_space_m, only:object, game_space
use input_handler_m
use vector2_m
use snake_tail_m, only:snake_tail

    type, extends(object) :: player
        type(vector2) :: move_vec
        class(snake_tail), pointer :: tail
        class(snake_tail), pointer :: head
        procedure(), pointer, nopass :: game_over
    contains
        procedure :: on_update => player_on_update
        procedure :: initialize => player_initialize
        procedure :: on_destroy => player_destroy
        procedure :: setup
    end type


contains

    subroutine setup(self, fruit_obj, loop, go)
    use fruit_m, only:fruit
    use object_action_m, only:object_action_callback, object_action
    implicit none
        class(player), target :: self
        class(fruit) :: fruit_obj
        type(object_action), intent(inout) :: loop
        procedure(), pointer, intent(in) :: go
        procedure(object_action_callback), pointer :: p => add_tail
        class(object), pointer :: o
        o => self
        call fruit_obj%on_eat%add_action(o, p)

        p=>player_move
        call loop%add_action(o, p)

        self%game_over => go

    end subroutine


    subroutine player_on_update(self, input)
    implicit none
        class(player), target, intent(inout) :: self
        integer  :: input

        select case(get_character())
            case('a')
                if(self%move_vec%x /= 1) then
                    self%move_vec = 0
                    self%move_vec%x = -1
                end if
            case('d')
                if(self%move_vec%x /= -1) then
                    self%move_vec = 0
                    self%move_vec%x = 1
                end if
            case('s')
                if(self%move_vec%y /= -1) then
                    self%move_vec = 0
                    self%move_vec%y = 1
                end if
            case('w')
                if(self%move_vec%y /= 1) then
                    self%move_vec = 0
                    self%move_vec%y = -1
                end if
        end select
    end subroutine

    subroutine player_initialize(self, parent)
    use object_action_m, only:object_action_callback
    implicit none
        class(player), intent(inout), target :: self
        class(game_space) :: parent

        self%glyph = '@'
        self%color%text = 7
        self%pos%x = self%parent%bounds%x/2
        self%pos%y = self%parent%bounds%y/2

        self%head => null()
        self%tail => null()

    end subroutine

    subroutine player_destroy(self)
    implicit none
        class(player), target, intent(inout) :: self
    end subroutine


    subroutine player_move(o, input)
    implicit none
        class(object) :: o
        integer :: input
        class(player), pointer :: self
        type(vector2) :: oldpos

        select type(o)
            type is(player)
                self => o
        end select
        oldpos = self%pos
        self%pos = self%pos + self%move_vec
        if(self%pos%x > self%parent%bounds%x) then
            self%pos%x = 1
        end if
        if(self%pos%y > self%parent%bounds%y) then
            self%pos%y = 1
        end if
        if(self%pos%x < 1) then
            self%pos%x = self%parent%bounds%x
        end if
        if(self%pos%y < 1) then
            self%pos%y = self%parent%bounds%y
        end if

        if(.not. associated(self%tail)) then
            return
        end if

        self%tail%pos = oldpos
        if(.not. associated(self%head)) then
            return
        end if
        self%head%head => self%tail
        self%head => self%tail
        self%tail => self%head%head
        self%head%head => null()

    end subroutine

    subroutine add_tail(s, input)
    implicit none
        class(object) :: s
        integer :: input
        class(player), pointer :: self
        class(snake_tail), pointer :: new_tail

        select type(s)
            type is(player)
                self => s
        end select

        if(.not. associated(self%tail)) then
            allocate(self%tail)
            self%tail%pos = self%pos
            self%tail%game_over => self%game_over
            call self%parent%add_object(self%tail)
            return
        end if

        if(.not. associated(self%head)) then
            self%head => self%tail
        end if

        allocate(new_tail)
        new_tail%game_over => self%game_over
        new_tail%head => self%tail
        new_tail%pos = self%tail%pos
        self%tail => new_tail
        call self%parent%add_object(new_tail)
        

    end subroutine

end module