! Test: using select type twice on the same class(*) allocatable array
! preserves data written in the first block.
program select_type_40
    implicit none
    class(*), allocatable :: v(:)

    ! Test with integer
    allocate(integer :: v(2))
    select type (v)
    type is (integer)
        v(1) = 42
        v(2) = 7
    end select
    select type (v)
    type is (integer)
        if (v(1) /= 42) error stop
        if (v(2) /= 7) error stop
    end select
    deallocate(v)

    ! Test with real
    allocate(real :: v(1))
    select type (v)
    type is (real)
        v(1) = 3.14
    end select
    select type (v)
    type is (real)
        if (abs(v(1) - 3.14) > 1.0e-5) error stop
    end select
    deallocate(v)

    print *, "PASS"
end program
