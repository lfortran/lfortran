program select_type_34
    implicit none

    type :: any_matrix
        class(*), allocatable :: value(:,:)
    end type

    call test_integer()
    call test_real()

contains

    subroutine test_integer()
        type(any_matrix) :: m
        integer, allocatable :: val(:,:)
        allocate(integer :: m%value(2,3))
        allocate(val(2,3))
        select type (v => m%value)
        type is (integer)
            v(1,1) = 10
            v(2,1) = 20
            v(1,2) = 30
            v(2,2) = 40
            v(1,3) = 50
            v(2,3) = 60
            val = v
        end select
        if (val(1,1) /= 10) error stop
        if (val(2,1) /= 20) error stop
        if (val(1,2) /= 30) error stop
        if (val(2,2) /= 40) error stop
        if (val(1,3) /= 50) error stop
        if (val(2,3) /= 60) error stop
    end subroutine

    subroutine test_real()
        type(any_matrix) :: m
        real, allocatable :: val(:,:)
        allocate(real :: m%value(2,2))
        allocate(val(2,2))
        select type (v => m%value)
        type is (real)
            v(1,1) = 1.5
            v(2,1) = 2.5
            v(1,2) = 3.5
            v(2,2) = 4.5
            val = v
        end select
        if (abs(val(1,1) - 1.5) > 1.0e-6) error stop
        if (abs(val(2,1) - 2.5) > 1.0e-6) error stop
        if (abs(val(1,2) - 3.5) > 1.0e-6) error stop
        if (abs(val(2,2) - 4.5) > 1.0e-6) error stop
    end subroutine

end program
