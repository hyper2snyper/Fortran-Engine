module game_space_m
use screen_m


    type :: sprite_t
        character(len=100) :: sprite = ""
        type(vector2) :: center
        type(vector2) :: bounds
    contains
    end type

    type :: object
        class(game_space), pointer :: parent

        character :: glyph = char(0)
        type(color_pair) :: color
        type(vector2) :: pos
        integer :: rotation = 0

        type(sprite_t) :: multi_sprite
        logical :: use_multi_sprite = .false.

        procedure(object_on_update), pointer :: on_update => null()
        procedure(object_initialize), pointer :: initialize => null()
        procedure(object_on_delete), pointer :: on_destroy => null()
    contains
        procedure :: delete
        procedure :: draw
    end type


    type :: object_container
        class(object), pointer :: item
    contains
    end type


    type, extends(window) :: game_space
        type(object_container), dimension(:), allocatable :: objects
        integer :: size=2, index=1
    contains
        procedure :: refresh => game_space_refresh
        procedure :: add_object
        procedure :: remove_object
    end type


    interface
    subroutine object_on_update(self, input)
    import object
    implicit none
        class(object) :: self
        integer :: input
    end subroutine
    subroutine object_initialize(self, parent)
    import object, game_space
    implicit none
        class(object) :: self
        class(game_space) :: parent
    end subroutine
    subroutine object_on_delete(self)
    import object
    implicit none
        class(object) :: self
    end subroutine
    end interface



contains

!object
    subroutine delete(self)
    implicit none
        class(object), target :: self
        if(associated(self%on_destroy)) then
            call self%on_destroy()
        end if
        call self%parent%remove_object(self)
    end subroutine



    subroutine draw(self, canvas, colors, canvas_bounds)
    implicit none
        class(object) :: self
        type(color_pair), dimension(:,:), allocatable :: colors
        character, dimension(:,:), allocatable :: canvas
        type(vector2) :: canvas_bounds

        if(self%glyph == char(0) .and. .not. self%use_multi_sprite) then
            return
        end if
        if(.not. self%use_multi_sprite) then
            if(self%pos%x > canvas_bounds%x .or. self%pos%x < 0) then
                return
            end if
            if(self%pos%y > canvas_bounds%y .or. self%pos%y < 0) then
                return
            end if
            canvas(self%pos%x, self%pos%y) = self%glyph
            if(self%color%text /= -1) then
                colors(self%pos%x, self%pos%y)%text = self%color%text
            end if
            if(self%color%background /= -1) then
                colors(self%pos%x, self%pos%y)%background = self%color%background
            end if
            return
        end if
        



    end subroutine



!game_space

subroutine game_space_refresh(self, input)
implicit none
    class(game_space) :: self
    integer :: input
    integer :: i
    class(object), pointer :: o
    

    if(.not. allocated(self%objects)) then
        return
    end if

    do i=1, self%index
        o => self%objects(i)%item
        call o%draw(self%window_canvas, self%window_colors, self%bounds)
        if(associated(o%on_update)) then
            call o%on_update(input)
        end if
    end do
    


end subroutine

subroutine add_object(self, object_to_add)
implicit none
    class(game_space) :: self
    class(object), pointer :: object_to_add
    type(object_container), dimension(:), allocatable :: new_objects
    integer :: i

    if(.not. allocated(self%objects)) then
        allocate(self%objects(self%size))
        self%objects(1)%item => object_to_add
        if(associated(object_to_add%initialize)) then
            call object_to_add%initialize(self)
        end if
        return
    end if

    self%index = self%index+1

    if(self%index > self%size) then
        allocate(new_objects(self%size*2))
        do i=1, self%size
            new_objects(i)%item => self%objects(i)%item
        end do
        self%size = self%size*2
        deallocate(self%objects)
        self%objects = new_objects
    end if

    self%objects(self%index)%item => object_to_add
    if(associated(object_to_add%initialize)) then
        call object_to_add%initialize(self)
    end if


end subroutine

subroutine remove_object(self, object_to_remove)
implicit none
    class(game_space) :: self
    class(object), target :: object_to_remove
    integer :: i, j


    if(.not. allocated(self%objects)) then
        return
    end if

    if(self%index == 0) then
        return
    end if

    do i=1, self%index
        if(associated(self%objects(i)%item, object_to_remove)) then
            self%index = self%index-1
            do j=i, self%index
                self%objects(j)%item => self%objects(j+1)%item
            end do
        end if
    end do



end subroutine

end module
