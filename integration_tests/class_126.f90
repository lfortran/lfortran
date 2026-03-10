! Test: passing a derived type array to a class(*) array parameter.
! This used to segfault in LLVM codegen (store_intrinsic_type_vptr
! was called for struct types in the unlimited polymorphic array path).
program class_126
    implicit none

    type :: point
        real :: x, y
    end type

    type :: container
        class(*), allocatable :: value(:)
    end type

    type(container) :: v

    call set_value(v, [point(1.0, 2.0)])

    select type (val => v%value)
    type is (point)
        if (abs(val(1)%x - 1.0) > 1e-6) error stop
        if (abs(val(1)%y - 2.0) > 1e-6) error stop
    class default
        error stop
    end select

    print *, "PASS"

contains

    subroutine set_value(this, value)
        class(container), intent(out) :: this
        class(*), intent(in) :: value(:)
        this%value = value
    end subroutine

end program
