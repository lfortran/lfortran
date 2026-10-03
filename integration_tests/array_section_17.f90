! Test: non-contiguous assumed-shape array passed to assumed-size dummy
! When a non-contiguous array section (e.g., a(1,:)) is passed through
! an assumed-shape argument to an assumed-size dummy, the compiler must
! create a contiguous temporary (copy-in).
program array_section_17
    implicit none
    integer :: a(2,3)
    a(1,:) = [10, 20, 30]
    a(2,:) = 0
    call check_via_assumed_shape(a(1,:))
contains
    subroutine check_assumed_size(x, n)
        integer, intent(in) :: x(*)
        integer, intent(in) :: n
        if (x(1) /= 10) error stop
        if (x(2) /= 20) error stop
        if (x(3) /= 30) error stop
    end subroutine
    subroutine check_via_assumed_shape(x)
        integer, intent(in) :: x(:)
        call check_assumed_size(x, size(x))
    end subroutine
end program
