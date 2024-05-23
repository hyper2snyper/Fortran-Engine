module dungeon_world
use game_space_m
use vector2_m
use dungeon_mob

    type :: tile
        logical :: passable = .true.
        character :: glyph = "."
    contains

    end type


    type :: level
        type(vector2) :: size
        type(tile), dimension(:,:), allocatable :: tiles
    contains
        procedure :: level_init
        procedure :: generate_level
    end type

    type, extends(game_space) :: world
        class(mob), pointer :: player

        integer :: count = 0
        integer :: current_level = 1
        type(level), dimension(:), allocatable :: levels

        type(vector2) :: camera_pos
    contains
        procedure :: refresh => world_refresh
        procedure :: world_init
        procedure :: generate_world
    end type

contains

!LEVEL

    subroutine level_init(self, xmax, ymax)
    implicit none
        class(level) :: self
        integer, intent(inout) :: xmax, ymax

        integer :: x,y
        real :: r
        logical :: wall
        
        self%size%x = xmax
        self%size%y = ymax
        allocate(self%tiles(xmax,ymax))

        do y=1, self%size%y
            do x=1, self%size%x
                r = rand()
                r = r*100000
                wall = mod(int(r),2) == 1
                if(.not. wall) then
                    cycle
                end if
                self%tiles(x,y)%glyph = '#'
                self%tiles(x,y)%passable = .false.
            end do
        end do

    end subroutine

    subroutine generate_level(self, smooth_passes)
    implicit none
        class(level) :: self
        integer :: smooth_passes
        integer :: x,y , j,k
        integer :: wall_count=0
        type(tile), allocatable, dimension(:,:) :: n_tiles

        if(smooth_passes <= 0) then
            return
        end if

        allocate(n_tiles(self%size%x, self%size%y))
        n_tiles(:,:) = self%tiles(:,:)

        do y=1, self%size%y
            do x=1, self%size%x
                wall_count = 0
                do j=-1, 1
                    do k=-1, 1
                        if(x+k <= 0 .or. x+k > self%size%x .or. y+j <= 0 .or. y+j > self%size%y) then
                            cycle
                        end if
                        if(self%tiles(x+k, y+j)%passable .or. (j==0 .and. k==0)) then
                            cycle
                        end if
                        wall_count = wall_count+1
                    end do
                end do
                if(wall_count == 4 .or. wall_count == 5) then
                    cycle
                end if
                if(wall_count > 5) then
                    n_tiles(x,y)%glyph = '#'
                    n_tiles(x,y)%passable = .false.
                    cycle
                end if
                n_tiles(x,y)%glyph = '.'
                n_tiles(x,y)%passable = .true.
            end do
        end do

        deallocate(self%tiles)
        self%tiles = n_tiles

        call self%generate_level(smooth_passes-1)

    end subroutine

!WORLD


    subroutine world_refresh(self, input)
    implicit none
        class(world) :: self
        integer :: input
        integer :: x,y
        type(vector2) :: offset

        class(level), allocatable :: clevel

        call self%window_clear()

        offset%x = self%player%pos%x - self%bounds%x/2
        offset%y = self%player%pos%y - self%bounds%y/2

        clevel = self%levels(self%current_level)
        do y=1, self%bounds%y
            do x=1, self%bounds%x
                self%window_canvas(x,y) = clevel%tiles(x+offset%x,y+offset%y)%glyph
            end do
        end do


    end subroutine


    subroutine world_init(self, x, y, l)
    implicit none
        class(world) :: self
        integer :: x, y, l
        integer :: i

        allocate(self%levels(l))
        self%count = l

        do i=1, self%count
            call self%levels(i)%level_init(x, y)
        end do

        call self%add_object(self%player)

    end subroutine

    subroutine generate_world(self)
    implicit none
        class(world) :: self
        integer :: i

        do i=1, self%count
            call self%levels(i)%generate_level(2)
        end do

    end subroutine




end module