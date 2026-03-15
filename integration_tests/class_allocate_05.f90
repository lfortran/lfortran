program class_allocate_05
    ! Test: assigning class(*) allocatable component to class(*) allocatable
    ! variable (polymorphic sourced allocation via intrinsic assignment).
    implicit none
    type :: box
        class(*), allocatable :: value
    end type
    type(box) :: x
    class(*), allocatable :: val

    x%value = 42
    val = x%value
    select type (val)
    type is (integer)
        if (val /= 42) error stop
    class default
        error stop
    end select

    x%value = 3.14
    val = x%value
    select type (val)
    type is (real)
        if (abs(val - 3.14) > 1.0e-5) error stop
    class default
        error stop
    end select

    x%value = .true.
    val = x%value
    select type (val)
    type is (logical)
        if (.not. val) error stop
    class default
        error stop
    end select

    print *, "PASS"
end program
