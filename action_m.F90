module action_m

    type, public :: action
        integer :: size, index = 1
        type(action_container), dimension(:), allocatable :: action_list
    contains
        procedure :: invoke
        procedure :: add_action
        procedure :: remove_action
    end type

    type :: action_container
        procedure(action_callback), pointer, nopass :: callback => null()
    contains
    end type
    
    interface
        subroutine action_callback(return)
        implicit none
        integer :: return
        end subroutine
    end interface

contains

    subroutine invoke(self, argument)
    implicit none
        class(action) :: self
        integer :: argument
        integer :: i
        if(.not. allocated(self%action_list)) then
            return
        end if
        do i=1, self%index
            call self%action_list(i)%callback(argument)
        end do
    end subroutine

    subroutine add_action(self, new_action)
    implicit none
        class(action) :: self
        procedure(action_callback), pointer :: new_action
        type(action_container), dimension(:), allocatable :: new_action_list
        integer :: i

        if(.not. allocated(self%action_list)) then
            self%size = 2
            allocate(self%action_list(self%size))
            self%action_list(1)%callback => new_action
            return
        end if

        self%index = self%index + 1
        if(self%index > self%size) then
            allocate(new_action_list(self%size*2))
            do i=1, self%size
                new_action_list(i) = self%action_list(i)
            end do
            self%size = self%size * 2
            new_action_list(self%index)%callback => new_action
            deallocate(self%action_list)
            self%action_list = new_action_list
            return
        end if
        self%action_list(self%index)%callback => new_action

    end subroutine

    subroutine remove_action(self, action_to_remove)
    implicit none
        class(action) :: self
        procedure(action_callback), pointer :: action_to_remove
        integer :: i, k

        do i=1, self%index
            if(associated(self%action_list(i)%callback, action_to_remove)) then
                self%index = self%index-1
                do k=i, self%index
                    self%action_list(k)%callback => self%action_list(k+1)%callback
                end do
                return
            end if
        end do

    end subroutine

end module