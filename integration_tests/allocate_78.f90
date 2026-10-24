module allocate_78_mod
    implicit none
    integer, parameter :: wp = selected_real_kind(15)
end module

program allocate_78
    ! A typed allocation takes its kind from a constant expression, not
    ! only from an integer literal
    use allocate_78_mod, only: wp
    implicit none
    integer, parameter :: dp = kind(1.d0), ik = 8
    class(*), allocatable :: x, a(:)

    allocate(real(dp) :: x)
    select type (x)
    type is (real(8))
        x = 1.0_8 / 3
        if (abs(x - 1.0_8 / 3) > 1e-15_8) error stop
    class default
        error stop
    end select
    deallocate(x)

    allocate(real(kind=dp) :: x)
    select type (x)
    type is (real(8))
    class default
        error stop
    end select
    deallocate(x)

    allocate(integer(ik) :: x)
    select type (x)
    type is (integer(8))
        x = 2_8**40
        if (x /= 1099511627776_8) error stop
    class default
        error stop
    end select
    deallocate(x)

    allocate(complex(kind(1.d0)) :: x)
    select type (x)
    type is (complex(8))
    class default
        error stop
    end select
    deallocate(x)

    allocate(logical(kind=ik) :: x)
    select type (x)
    type is (logical(8))
    class default
        error stop
    end select
    deallocate(x)

    allocate(real(wp) :: a(3))
    select type (a)
    type is (real(8))
        a = 1.5_8
        if (size(a) /= 3) error stop
        if (any(a /= 1.5_8)) error stop
    class default
        error stop
    end select
    print *, "ok"
end program
