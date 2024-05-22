module tank_player
use game_space_m
use label_m

    type, extends(object) :: player 
        integer :: team = 0
        integer :: health = 5
        integer :: walls = 3
        integer :: player_num

        type(vector2) :: dir

        class(label), pointer :: score_card

        integer :: last_fired = 0
        integer :: fire_delay = 30

        character(len=6) :: controls
    
    contains
        procedure :: initialize => player_init
        procedure :: on_update => player_update
        procedure :: on_destroy => player_delete
        procedure :: set_team
        procedure :: set_control
        procedure :: setup
        procedure :: refresh_score
        procedure :: take_damage
        procedure :: move
        procedure :: turn
    end type

contains

    subroutine move(self, a)
    implicit none
        class(player) :: self
        integer :: a
        type(vector2) :: npos
        class(object), pointer :: o

        npos%x = self%pos%x+(self%dir%x*a)
        npos%y = self%pos%y+(self%dir%y*a)

        if(npos%x <= 1 .or. npos%x > self%parent%bounds%x-1 .or. npos%y <= 1 .or. npos%y > self%parent%bounds%y-1) then
            return
        end if

        self%pos = npos

    end subroutine

    subroutine turn(self, r)
    implicit none
        class(player) :: self
        integer :: r

        self%multi_sprite%rotation = self%multi_sprite%rotation+r
        if(self%multi_sprite%rotation > 3) then
            self%multi_sprite%rotation = 0
        end if
        if(self%multi_sprite%rotation < 0) then
            self%multi_sprite%rotation = 3
        end if

        self%dir = 0
        select case(self%multi_sprite%rotation)
            case(0)
                self%dir%y=-1
            case(1)
                self%dir%x=1
            case(2)
                self%dir%y=1
            case(3)
                self%dir%x=-1
        end select

    end subroutine

    subroutine take_damage(self, damage)
    use tanks_main, only:game_over
    implicit none
        class(player) :: self
        integer :: damage

        self%health = self%health - damage
        call self%refresh_score()

        if(self%health > 0) then
            return
        end if

        call self%delete()
        call game_over(self%player_num)

    end subroutine

    subroutine refresh_score(self)
    implicit none
        class(player) :: self
        character(len=20) :: msg

        write(msg, '(I1)') self%player_num
        msg = "Player "//msg
        call self%score_card%set_message(msg, 1)
        write(msg, '(I2)') self%health
        msg = "Health: "//msg
        call self%score_card%set_message(msg, 2)
        write(msg, '(I2)') self%walls
        msg = "Walls: "//msg
        call self%score_card%set_message(msg, 3)

    end subroutine

    subroutine setup(self, score, player_num)
    implicit none
        class(player) :: self
        class(label), pointer :: score
        integer :: player_num

        self%player_num = player_num
        self%score_card => score

        self%score_card%color_fill%background = self%team
        self%score_card%color_fill%text = 7

        call self%refresh_score()
    end subroutine

    subroutine set_control(self, controls)
    implicit none
        class(player) :: self
        integer :: controls

        if(controls == 1) then
            self%controls = "wsad z"
            return
        end if
        self%controls = "ujhkbn"
    end subroutine

    subroutine set_team(self, team)
    implicit none
        class(player) :: self
        integer :: team

        self%color%text = team
        self%team = team

    end subroutine

    subroutine player_init(self, parent)
    implicit none
        class(player), target, intent(inout) :: self
        class(game_space), pointer :: parent

        self%use_multi_sprite = .true.
        self%multi_sprite%sprite = "#*##H##$#"
        self%multi_sprite%bounds%x = 3
        self%multi_sprite%bounds%y = 3
        self%multi_sprite%center%x = 2
        self%multi_sprite%center%y = 2

        self%color%background = 0

        self%dir%y = -1
        if(self%multi_sprite%rotation == 2) then
            self%dir%y = 1
        end if

    end subroutine

    subroutine player_update(self, input)
    use input_handler_m
    use tank_bullet, only:bullet
    use tank_wall, only:wall
    implicit none
        class(player), target, intent(inout) :: self
        integer :: input
        class(bullet), pointer :: b
        class(wall), pointer :: w
        character :: c
        c = get_character()
        if(c == self%controls(5:5) .and. self%last_fired+self%fire_delay <= input) then
            self%last_fired = input
            allocate(b)
            b%team = self%team
            b%dir = self%dir
            b%pos = self%pos
            b%pos%x = b%pos%x + self%dir%x
            b%pos%y = b%pos%y + self%dir%y
            call self%parent%add_object(b)
        end if

        if(c == self%controls(3:3)) then
            call self%turn(-1)
        end if
        if(c == self%controls(4:4)) then
            call self%turn(1)
        end if

        if(c == self%controls(1:1)) then
            call self%move(1)
        end if

        if(c == self%controls(2:2)) then
            call self%move(-1)
        end if

        if(c == self%controls(6:6) .and. self%walls > 0) then
            allocate(w)
            call w%set_team(self%team)
            w%pos = self%pos+self%dir
            w%multi_sprite%rotation = self%multi_sprite%rotation
            call self%parent%add_object(w)
            self%walls = self%walls-1
            call self%refresh_score()
        end if

    end subroutine

    subroutine player_delete(self)
    implicit none
        class(player), target, intent(inout) :: self
    end subroutine

end module