module selection_m
use screen_m

#define MAX_LIST_LEN 50

    type :: selection_container
        character(len=50) :: text
        procedure(selection_action), pointer, nopass :: on_selection => null()
    contains
    end type


    type, extends(window) :: selection
        integer :: selection = 1
        integer :: size = 0
        type(selection_container), dimension(:) :: selection_list(MAX_LIST_LEN)


    contains
        procedure :: refresh => selection_refresh
        procedure :: add_selection
    end type


    interface
        subroutine selection_action(selection_box, index)
        import selection
        implicit none
            class(selection) :: selection_box
            integer :: index
        end subroutine
    end interface


contains

    subroutine selection_refresh(self, input)
    use input_handler_m, only:get_character
    implicit none
        class(selection) :: self
        integer :: input
        integer :: i, j
        character :: inputted_key

        if(self%size == 0) then
            return
        end if

        do i=1, self%size
            if(self%selection == i) then
                self%window_canvas(2, i+1) = '>'
            end if
            do j=1, self%bounds%x-3
                if(self%selection_list(i)%text(j:) == char(0)) then
                    exit
                end if
                self%window_canvas(j+3, i+1) = self%selection_list(i)%text(j:)
            end do
        end do

        inputted_key = get_character()
        if(inputted_key == 'j' .and. self%selection < self%size) then
            self%selection = self%selection+1
        end if

        if(inputted_key == 'u' .and. self%selection > 1) then
            self%selection = self%selection-1
        end if

        if(inputted_key == char(10)) then
            if(associated(self%selection_list(self%selection)%on_selection)) then
                call self%selection_list(self%selection)%on_selection(self, self%selection)
            end if
        end if


    end subroutine

    subroutine add_selection(self, text, action)
    implicit none
        class(selection) :: self
        character(len=*) :: text
        procedure(selection_action), pointer, optional :: action

        character(len=50) :: parsed_text
        
        if(self%size > MAX_LIST_LEN) then
            return
        end if

        self%size = self%size + 1
        parsed_text = trim(text)//char(0)
        self%selection_list(self%size)%text = parsed_text
        if(present(action)) then
            self%selection_list(self%size)%on_selection => action
        end if
       
    end subroutine


end module