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
        character :: fill_char = ' '
        type(color_pair) :: color_fill

        logical :: border = .true.

        character, allocatable, dimension(:,:) :: window_canvas
        type(color_pair), allocatable, dimension(:,:) :: window_colors

    contains
        procedure :: window_refresh
        procedure(window_refresh_), deferred :: refresh
        procedure :: window_clear
        procedure :: set_size
        procedure :: set_color
        procedure :: draw_border
        procedure :: fill_section
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

    subroutine window_refresh(self, input)
    implicit none
        class(window) :: self
        integer :: input

        call self%window_clear()
        if(self%border) then
            call self%draw_border()
        end if

        call self%refresh(input)
    end subroutine


    subroutine window_clear(self)
    implicit none
        class(window) :: self
        if(.not. allocated(self%window_canvas)) then
            return
        end if
        self%window_canvas(:,:) = self%fill_char
        self%window_colors(:,:) = self%color_fill
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

        if(.not. allocated(self%window_canvas)) then
            return
        end if

        self%window_canvas(1,:) = '|'
        self%window_canvas(self%bounds%x,:) = '|'
        self%window_canvas(:,1) = '-'
        self%window_canvas(:,self%bounds%y) = '-'
        self%window_canvas(1,1) = '+'
        self%window_canvas(1,self%bounds%y) = '+'
        self%window_canvas(self%bounds%x,1) = '+'
        self%window_canvas(self%bounds%x,self%bounds%y) = '+'
    
    end subroutine



    subroutine fill_section(self, char, x, y)
        implicit none
            class(window) :: self
            character :: char
            integer, optional :: x, y
            integer :: x_=0, y_=0
    
            if(present(x)) then
                x_ = x
            end if
            if(present(y)) then
                y_ = y
            end if
            
            if(x_ == 0 .and. y_ == 0) then
                self%window_canvas(:,:) = char
                return
            end if
    
            if(x_ == 0 ) then
                self%window_canvas(:,y_) = char
                return
            end if
    
            self%window_canvas(x_,:) = char
    
    
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

        if(.not. allocated(self%windows)) then
            return
        end if

        do i=1, self%index
            call self%windows(i)%instance%window_refresh(input)
            do x=1, self%windows(i)%instance%bounds%x
                do y=1, self%windows(i)%instance%bounds%y
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
            allocate(self%windows(self%size))
            self%windows(self%index)%instance => new_window
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
