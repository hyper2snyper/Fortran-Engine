module screen_m
    use vector2_m

    type :: color_pair
        integer :: text = 7
        integer :: background = 0
    end type

    type :: window_container
        class(window), pointer :: instance
    contains
    end type

    type, abstract :: window
        type(vector2) :: pos
        type(vector2) :: bounds
        class(screen), pointer :: parent

        character, allocatable, dimension(:,:) :: window_canvas
        type(color_pair), allocatable, dimension(:,:) :: window_colors

    contains
        procedure(window_refresh_), deferred :: refresh
        procedure :: window_clear
        procedure :: set_size
        procedure :: set_color
        procedure :: draw_border
    end type


    type, public :: screen
        type(window_container), dimension(:), allocatable :: windows
        integer :: size=2, index=1

        character, allocatable, dimension(:,:) :: canvas
        type(color_pair), allocatable, dimension(:,:) :: canvas_colors
        type(vector2) :: canvas_size

    contains
        procedure :: refresh
        procedure :: add_window
        procedure :: remove_window

        procedure :: screen_write
        procedure :: screen_clear

    end type


    interface
        subroutine window_refresh_(self, input)
        import window
        implicit none
            class(window) :: self
            integer :: input
        end subroutine
    end interface

contains

    !window

    subroutine window_clear(self)
    implicit none
        class(window) :: self
        if(.not. allocated(self%window_canvas)) then
            return
        end if
        self%window_canvas(:,:) = ' '
    end subroutine
    
    subroutine set_size(self, new_bounds)
    implicit none
        class(window) :: self
        type(vector2) :: new_bounds

        if(self%bounds%x > 0 .or. self%bounds%y > 0) then
            deallocate(self%window_canvas)
            deallocate(self%window_colors)
        end if

        self%bounds = new_bounds
        allocate(self%window_canvas(new_bounds%x,new_bounds%y))
        allocate(self%window_colors(new_bounds%x,new_bounds%y))
        call self%window_clear()
    end subroutine

    subroutine set_color(self, text_color, background_color)
    implicit none
        class(window) :: self
        type(color_pair) :: color
        integer :: text_color, background_color
        color%text = text_color
        color%background = background_color
        self%window_colors(:,:) = color
    end subroutine

    subroutine draw_border(self)
    implicit none
        class(window) :: self

        self%window_canvas(1,:) = '-'
        self%window_canvas(self%bounds%x,:) = '-'
        self%window_canvas(:,1) = '|'
        self%window_canvas(:,self%bounds%y) = '|'
        self%window_canvas(1,1) = '+'
        self%window_canvas(1,self%bounds%y) = '+'
        self%window_canvas(self%bounds%x,1) = '+'
        self%window_canvas(self%bounds%x,self%bounds%y) = '+'
    
    end subroutine

    !screen

    subroutine screen_write(self, char, pos)
    implicit none
        class(screen) :: self
        character :: char
        type(vector2) :: pos
        self%canvas(pos%x, pos%y) = char
    end subroutine

    subroutine screen_clear(self)
    implicit none
        class(screen) :: self
        self%canvas(:,:) = ' '
    end subroutine


    subroutine refresh(self, input)
    implicit none
        class(screen) :: self
        integer :: input, i
        integer :: x,y
        type(color_pair) :: color
        call self%screen_clear()

        do i=1, self%index
            call self%windows(i)%instance%refresh(input)
            do y=1, self%windows(i)%instance%bounds%y
                do x=1, self%windows(i)%instance%bounds%x
        color = self%windows(i)%instance%window_colors(x,y)
        self%canvas_colors(self%windows(i)%instance%pos%x+x,self%windows(i)%instance%pos%y+y) = color
        !Do not tab, there is a limit to line length for some reason 
        self%canvas(self%windows(i)%instance%pos%x+x,self%windows(i)%instance%pos%y+y) = self%windows(i)%instance%window_canvas(x,y)
                end do
            end do
        end do
    end subroutine

    subroutine add_window(self, new_window)
    implicit none
        class(screen), target :: self
        class(window), target :: new_window
        type(window_container), dimension(:), allocatable :: new_windows
        integer :: i

        new_window%parent => self

        if(.not. allocated(self%windows)) then
            allocate(self%windows(2))
            self%windows(1)%instance => new_window
            return
        end if

        self%index = self%index+1
        if(self%index > self%size) then
            allocate(new_windows(self%size*2))
            do i=1, self%size
                new_windows(i) = self%windows(i)
            end do
            self%size = self%size*2
            new_windows(self%index)%instance => new_window
            deallocate(self%windows)
            self%windows = new_windows
            return
        end if

        self%windows(self%index)%instance => new_window

    end subroutine

    subroutine remove_window(self, window_to_remove)
    implicit none
        class(screen) :: self
        class(window), pointer :: window_to_remove
        integer :: i, j

        if(.not. allocated(self%windows)) then
            return
        end if

        do i=1, self%index
            if(same_type_as(self%windows(i)%instance, window_to_remove)) then
                self%index = self%index-1
                do j=i, self%index
                    self%windows(j) = self%windows(j+1)
                end do
                return
            end if
        end do


    end subroutine

end module
