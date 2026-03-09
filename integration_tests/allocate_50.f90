module allocate_50_mod
    implicit none
    type :: any_scalar
        class(*), allocatable :: value
    contains
        procedure :: set_value => any_scalar_set_value
    end type
    type :: any_vector
        class(*), allocatable :: value(:)
    contains
        procedure :: set_value => any_vector_set_value
    end type
contains
    subroutine any_scalar_set_value(this, val)
        class(any_scalar), intent(out) :: this
        class(*), intent(in) :: val
        allocate(this%value, source=val)
    end subroutine
    subroutine any_vector_set_value(this, val)
        class(any_vector), intent(out) :: this
        class(*), intent(in) :: val(:)
        allocate(this%value, source=val)
    end subroutine
end module

program allocate_50
    use allocate_50_mod
    implicit none
    type(any_scalar) :: x
    type(any_vector) :: v
    integer :: src(3)

    call x%set_value(42)
    select type (val => x%value)
        type is (integer)
            if (val /= 42) error stop
        class default
            error stop
    end select

    src = [10, 20, 30]
    call v%set_value(src)
    select type (arr => v%value)
        type is (integer)
            if (size(arr) /= 3) error stop
            if (arr(1) /= 10) error stop
            if (arr(2) /= 20) error stop
            if (arr(3) /= 30) error stop
        class default
            error stop
    end select

    print *, "PASS"
end program
