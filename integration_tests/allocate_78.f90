program allocate_78
    use iso_fortran_env, only: int64
    implicit none
    integer, parameter :: k = 8
    integer, parameter :: dp = kind(1.d0)
    class(*), allocatable :: s, a(:)

    allocate(integer(k) :: s)
    select type (s)
    type is (integer(8))
    class default
        error stop 1
    end select
    deallocate(s)

    allocate(integer(int64) :: a(3))
    select type (a)
    type is (integer(8))
        if (size(a) /= 3) error stop 2
    class default
        error stop 3
    end select
    deallocate(a)

    allocate(real(kind=dp) :: s)
    select type (s)
    type is (real(8))
    class default
        error stop 4
    end select
    deallocate(s)

    allocate(real(kind(1.d0)) :: s)
    select type (s)
    type is (real(8))
    class default
        error stop 5
    end select
    deallocate(s)

    allocate(complex(dp) :: s)
    select type (s)
    type is (complex(8))
    class default
        error stop 6
    end select
    deallocate(s)

    allocate(logical(k) :: s)
    select type (s)
    type is (logical(8))
    class default
        error stop 7
    end select
    print *, "ok"
end program
