program main
    implicit none

    type item
        integer, allocatable :: values(:)
    end type item

    type(item) :: x

    x = item(null())

    print *, "ok"
end program main
