program test_nested_struct
    implicit none

    type :: Point
        real(8) :: x, y
    end type Point

    type :: Player
        integer :: health
        type(Point) :: pos
    end type Player

    type(Player) :: p

    if (this_image() == 1) then
        p%health = 100
        p%pos%x = 10.5_8
        p%pos%y = -42.1_8
    else
        p%health = 0
        p%pos%x = 0.0_8
        p%pos%y = 0.0_8
    end if

    call co_broadcast(p, 1)

    sync all

    if (p%health /= 100) error stop
    if (p%pos%x /= 10.5_8) error stop
    if (p%pos%y /= -42.1_8) error stop

end program test_nested_struct