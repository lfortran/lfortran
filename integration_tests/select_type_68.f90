program select_type_68
    ! Association names in SELECT TYPE and SELECT RANK are case-insensitive
    implicit none
    class(*), allocatable :: thing
    real :: a(2,3)

    allocate(thing, source=42)
    select type (Val => thing)
    type is (integer)
        if (Val /= 42) error stop
        if (vAL + 1 /= 43) error stop
    class default
        error stop
    end select

    a = 1.0
    call check_rank(a)
    print *, "OK"
contains
    subroutine check_rank(x)
        real, intent(in) :: x(..)
        select rank (Y => x)
        rank (2)
            if (size(Y, 1) /= 2) error stop
            if (size(y, 2) /= 3) error stop
            if (abs(sum(Y) - 6.0) > 1e-6) error stop
        rank default
            error stop
        end select
    end subroutine
end program
