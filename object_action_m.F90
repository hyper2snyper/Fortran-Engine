module object_action_m
use game_space_m


    type :: action_holder
        class(object), pointer :: object_to_call
        procedure(object_action_callback), pointer, nopass :: callback
    end type


    type :: object_action
        type(action_holder), dimension(:), allocatable :: contents
        integer :: size=2, index=1
    contains
        procedure :: add_action
        procedure :: remove_action
        procedure :: invoke
    end type



    interface 
    subroutine object_action_callback(self, input)
    import object
    implicit none
        class(object) :: self
        integer :: input
    end subroutine
    end interface

contains


    subroutine add_action(self, object_to_add, action, *)
    implicit none
        class(object_action) :: self
        class(object), pointer :: object_to_add
        procedure(object_action_callback), pointer :: action
        integer :: i
        type(action_holder), dimension(:), allocatable :: new_contents
        
        if(.not. allocated(self%contents)) then
            allocate(self%contents(self%size))
            self%contents(self%index)%callback => action
            self%contents(self%index)%object_to_call => object_to_add
            return
        end if

        self%index = self%index+1

        if(self%index > self%size) then
            allocate(new_contents(self%size*2))
            do i=1, self%size
                new_contents(i) = self%contents(i)
            end do
            self%size = self%size*2
            deallocate(self%contents)
            self%contents = new_contents
        end if
        self%contents(self%index)%callback => action
        self%contents(self%index)%object_to_call => object_to_add
    end subroutine


    subroutine remove_action(self, object_to_remove, action)
    implicit none
        class(object_action) :: self
        class(object), pointer :: object_to_remove
        procedure(object_action_callback), pointer :: action
        integer :: i, j

        if(self%index == 0) then
            return
        end if
        do i=1, self%index
            if(.not. associated(self%contents(i)%callback, action)) then
                cycle
            end if
            if(.not. associated(self%contents(i)%object_to_call, object_to_remove)) then
                cycle
            end if
            self%index = self%index-1
            do j=i, self%index
                self%contents(j) = self%contents(j+1)
            end do
            return
        end do


    end subroutine

    subroutine invoke(self, input)
    implicit none
        class(object_action) :: self
        integer :: input
        integer :: i
        if(self%index == 0) then
            return
        end if 
        do i=1, self%index
            call self%contents(i)%callback(self%contents(i)%object_to_call, input)
        end do

    end subroutine

end module