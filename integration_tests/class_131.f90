module class_131_mod
    implicit none

    type :: point
        real :: x, y
    end type

    type :: container
        class(*), allocatable :: value(:)
    contains
        procedure :: set_value
    end type
contains
    subroutine set_value(this, value)
        class(container), intent(out) :: this
        class(*), intent(in) :: value(:)
        this%value = value
    end subroutine
end module

program class_131
    use class_131_mod
    implicit none

    type(container) :: c
    type(point) :: pts(2)
    pts(1) = point(1.0, 2.0)
    pts(2) = point(3.0, 4.0)
    call c%set_value(pts)
    select type (v => c%value)
    type is (point)
        if (size(v) /= 2) error stop
        if (abs(v(1)%x - 1.0) > 1e-6) error stop
        if (abs(v(1)%y - 2.0) > 1e-6) error stop
        if (abs(v(2)%x - 3.0) > 1e-6) error stop
        if (abs(v(2)%y - 4.0) > 1e-6) error stop
    class default
        error stop
    end select
    print *, "PASS"
end program
