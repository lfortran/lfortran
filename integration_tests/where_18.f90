! A where construct that calls an internal function, which uses variables of
! the host (#14797).
program where_18
    implicit none
    integer, allocatable :: arr(:), b(:)
    integer :: n
    allocate(arr(3), b(3))
    arr = [1, 2, 3]
    b = 0
    n = 0
    where (arr > 1)
        arr = f()
        b = n
    end where
    if (any(arr /= [1, 5, 5])) error stop 1
    if (any(b /= [0, 3, 3])) error stop 2
    n = 10
    where (g(arr) > 12) arr = 0
    if (any(arr /= [1, 0, 0])) error stop 3
    where (arr > 0)
        arr = 2
    elsewhere
        arr = f()
    end where
    if (any(arr /= [2, 5, 5])) error stop 4
    print *, arr, b
contains
    integer function f()
        n = size(arr)
        f = n + 2
    end function
    elemental integer function g(x)
        integer, intent(in) :: x
        g = x + n
    end function
end program
