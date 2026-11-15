! Reallocation on intrinsic assignment is on by default, with no compiler
! option, for allocatable dummy arguments and function results.
module realloc_lhs_35_m
    implicit none
contains
    subroutine set_out(a, n)
        integer, allocatable, intent(out) :: a(:)
        integer, intent(in) :: n
        integer :: i
        a = [(10*i, i = 1, n)]
    end subroutine set_out

    subroutine grow(a)
        integer, allocatable, intent(inout) :: a(:)
        if (allocated(a)) then
            a = [a, size(a) + 1]
        else
            a = [1]
        end if
    end subroutine grow

    function make(n) result(r)
        integer, intent(in) :: n
        integer, allocatable :: r(:)
        integer :: i
        r = [(i, i = 1, n)]
        r = [r, r]
    end function make

    function squares(n) result(r)
        integer, intent(in) :: n
        real, allocatable :: r(:, :)
        integer :: i
        r = reshape([(real(i*i), i = 1, n*2)], [2, n])
    end function squares
end module realloc_lhs_35_m

program realloc_lhs_35
    use realloc_lhs_35_m, only: set_out, grow, make, squares
    implicit none
    integer, allocatable :: a(:), b(:)
    real, allocatable :: s(:, :)

    ! intent(out): deallocated on entry, then allocated by the assignment
    call set_out(a, 3)
    if (size(a) /= 3) error stop
    if (any(a /= [10, 20, 30])) error stop
    call set_out(a, 1)
    if (size(a) /= 1) error stop
    if (a(1) /= 10) error stop

    ! intent(inout), unallocated on entry, then grown
    call grow(b)
    if (size(b) /= 1) error stop
    call grow(b)
    call grow(b)
    if (size(b) /= 3) error stop
    if (any(b /= [1, 2, 3])) error stop

    ! Allocatable function result variables
    a = make(2)
    if (size(a) /= 4) error stop
    if (any(a /= [1, 2, 1, 2])) error stop
    s = squares(3)
    if (any(shape(s) /= [2, 3])) error stop
    if (s(2, 3) /= 36.0) error stop
    print *, a, b, s
end program realloc_lhs_35
