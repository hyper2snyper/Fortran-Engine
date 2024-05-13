module game_space_m
use screen_m

    type :: object
        class(game_space), pointer :: parent

        character :: glyph = ''

        procedure(object_on_update), pointer :: on_update => null()
        procedure(object_initialize), pointer :: initialize => null()
        procedure(object_on_delete), pointer :: on_destroy => null()
    contains
        procedure :: delete
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

!game_space

subroutine game_space_refresh(self, input)
implicit none
    class(game_space) :: self
    integer :: input
    integer :: i

    do i=1, self%index
        if(associated(self%objects(i)%item%on_update)) then
            call self%objects(i)%item%on_update(input)
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
