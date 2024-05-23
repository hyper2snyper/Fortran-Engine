module dungeon_main
use main_loop_m, only:main_loop
use dungeon_world
use screen_handler_m
use screen_m
use dungeon_player

    class(main_loop), pointer :: ml
    class(screen), allocatable :: ms
    class(player), pointer :: p

contains

    subroutine start(main_l)
    implicit none
        class(main_loop), pointer :: main_l
        class(world), pointer :: gworld
        class(screen), pointer :: main_s
        type(vector2) :: bounds
        integer :: f
        integer :: v(8)
        ml => main_l

        call date_and_time(values=v)
        f = v(8)
        call srand(f)

        ms = make_new_screen(100, 50)

        allocate(gworld)
        allocate(p)
        gworld%player => p
        p%pos = 100
        call gworld%world_init(200, 200, 3)
        call gworld%generate_world()
        bounds%x = 100
        bounds%y = 50
        call gworld%set_size(bounds)
        gworld%camera_pos%x = 100
        gworld%camera_pos%y = 100


        call ms%add_window(gworld)

        call add_screen(ms)
        call set_active_screen(ms)

    end subroutine


end module