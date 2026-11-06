! An allocatable actual of an optional, nonallocatable dummy of a function
! referenced inside WHERE: the dummy is present exactly when the actual is
! allocated.
module where_20_mod
    implicit none
contains
    integer function sum_or(p)
        integer, optional, intent(in) :: p(:)
        sum_or = -1
        if (present(p)) sum_or = sum(p)
    end function

    subroutine forward(n, a)
        integer, intent(in) :: n
        integer, allocatable, intent(in) :: a(:)
        integer :: r(3)
        r = 0
        where (r == 0) r = sum_or(a)
        if (any(r /= n)) error stop
        r = 0
        where (r /= 0)
            r = 5
        elsewhere
            r = sum_or(a) + 1
        end where
        if (any(r /= n + 1)) error stop
    end subroutine
end module

program where_20
    use where_20_mod
    implicit none
    integer, allocatable :: c(:)
    call forward(-1, c)
    allocate(c(2))
    c = [4, 5]
    call forward(9, c)
    print *, "ok"
end program
