module game_space_m
use screen_m

    !This is a multisprite holder
    type :: sprite_t
        character(len=100) :: sprite = ""
        type(vector2) :: center
        type(vector2) :: bounds
        !0-3 with 0 being up and 3 being left
        integer :: rotation = 0
    contains
    end type

    !Game objects. They recieve tick updates.
    type, abstract :: object
        class(game_space), pointer :: parent

        character :: glyph = char(0)
        type(color_pair) :: color
        type(vector2) :: pos

        type(sprite_t) :: multi_sprite
        logical :: use_multi_sprite = .false.

    contains
        procedure :: delete
        procedure :: draw
        procedure(object_on_update), deferred :: on_update
        procedure(object_initialize), deferred :: initialize
        procedure(object_on_delete), deferred :: on_destroy
        
    end type

    !No lists of abstract items, so container it is
    type :: object_container
        class(object), pointer :: item
    contains
    end type

    !The actual game space window. Holds objects and manages its canvas.
    type, extends(window) :: game_space
        type(object_container), dimension(:), allocatable :: objects
        integer :: size=2, index=1
    contains
        procedure :: refresh => game_space_refresh
        procedure :: add_object
        procedure :: remove_object
        procedure :: get_object_at_pos
    end type


    interface
    subroutine object_on_update(self, input)
    import object
    implicit none
        class(object), target, intent(inout) :: self
        integer :: input
    end subroutine
    subroutine object_initialize(self, parent)
    import object, game_space
    implicit none
        class(object), target, intent(inout) :: self
        class(game_space) :: parent
    end subroutine
    subroutine object_on_delete(self)
    import object
    implicit none
        class(object), target, intent(inout) :: self
    end subroutine
    end interface



contains

!object

    !deletes itself and removes it from its parent. Does not deallocate itself because I dont want to refactor the thing
    subroutine delete(self)
    implicit none
        class(object), target :: self
        call self%on_destroy()
        call self%parent%remove_object(self)
    end subroutine


    !Draws itself onto canvas. It's really big because of multi-sprite stuff.
    subroutine draw(self, canvas, colors, canvas_bounds)
    implicit none
        class(object) :: self
        type(color_pair), dimension(:,:), allocatable :: colors
        character, dimension(:,:), allocatable :: canvas
        type(vector2) :: canvas_bounds
        type(vector2) :: d_pos, s_pos
        integer :: x,y,c,v
        logical :: h


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

        select case(self%multi_sprite%rotation)
            case(0)
                v = 1
                s_pos%x = self%pos%x-self%multi_sprite%center%x
                s_pos%y = self%pos%y-self%multi_sprite%center%y
                h=.false.
            case(1)
                v = -1
                s_pos%x = self%pos%x+self%multi_sprite%center%x
                s_pos%y = self%pos%y+self%multi_sprite%center%y
                h=.true.
            case(2)
                v = -1
                s_pos%x = self%pos%x+self%multi_sprite%center%x
                s_pos%y = self%pos%y+self%multi_sprite%center%y
                h=.false.  
            case(3)
                v = 1
                s_pos%x = self%pos%x-self%multi_sprite%center%x
                s_pos%y = self%pos%y-self%multi_sprite%center%y
                h=.true.
                
        end select
        c=1
        do y=1, self%multi_sprite%bounds%y
            do x=1, self%multi_sprite%bounds%x
                if(h) then
                    d_pos%y = s_pos%y+(x*v)
                    d_pos%x = s_pos%x+(y*v)
                else
                    d_pos%x = s_pos%x+(x*v)
                    d_pos%y = s_pos%y+(y*v)
                end if

                if(d_pos%x < 1 .or. d_pos%x > canvas_bounds%x) then
                    c=c+1
                    cycle
                end if
                if(d_pos%y < 1 .or. d_pos%y > canvas_bounds%y) then
                    c=c+1
                    cycle
                end if
                canvas(d_pos%x, d_pos%y) = self%multi_sprite%sprite(c:)
                
                if(self%color%text /= -1) then
                    colors(d_pos%x, d_pos%y)%text = self%color%text  
                end if
                if(self%color%background /= -1) then
                    colors(d_pos%x, d_pos%y)%background = self%color%background  
                end if
                c=c+1
            end do
        end do
    end subroutine



!game_space

!Game space update, clears window, then calls draw on all objects.
subroutine game_space_refresh(self, input)
implicit none
    class(game_space) :: self
    integer :: input
    integer :: i
    class(object), pointer :: o

    call self%window_clear()
    

    if(.not. allocated(self%objects)) then
        return
    end if

    do i=1, self%index
        o => self%objects(i)%item
        call o%draw(self%window_canvas, self%window_colors, self%bounds)
        call o%on_update(input)
    end do
end subroutine

subroutine add_object(self, object_to_add)
implicit none
    class(game_space), target :: self
    class(object), pointer, intent(in) :: object_to_add
    type(object_container), dimension(:), allocatable :: new_objects
    integer :: i

    if(.not. allocated(self%objects)) then
        allocate(self%objects(self%size))
        self%objects(1)%item => object_to_add
        object_to_add%parent => self
        call object_to_add%initialize(self)
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
    object_to_add%parent => self
    call object_to_add%initialize(self)


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
            return
        end if
    end do
end subroutine

!Returns a pointer to an object at 'pos'. If excludes is set, then it ignores excludes.
function get_object_at_pos(self, pos, excludes) result(found)
implicit none
    class(game_space) :: self
    type(vector2) :: pos
    class(object), pointer, optional , intent(in):: excludes
    class(object), pointer :: found
    class(object), pointer :: c
    type(vector2) :: lb, hb
    integer :: i
    found => null()
    do i=1, self%index
        if(present(excludes)) then
            if(associated(self%objects(i)%item, excludes)) then
                cycle
            end if
        end if
        if(self%objects(i)%item%use_multi_sprite) then
            c => self%objects(i)%item
            lb = c%pos
            lb%x = lb%x-c%multi_sprite%center%x+1
            lb%y = lb%y-c%multi_sprite%center%y+1
            hb = c%pos
            hb%x = hb%x+c%multi_sprite%center%x-1
            hb%y = hb%y+c%multi_sprite%center%y-1
            if(pos%x < lb%x .or. pos%y < lb%y .or. pos%x > hb%x .or. pos%y > hb%y) then
                cycle
            end if
            found => c
            return
        end if
        if(self%objects(i)%item%pos%x /= pos%x .or. self%objects(i)%item%pos%y /= pos%y) then
            cycle
        end if
        found => self%objects(i)%item
        return
    end do
end function


end module
