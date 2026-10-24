! gpu decline reason: StructArrayConstant
! an element of a named constant array of a derived type passed as an
! actual argument in the loop body. Checked with --fast: --fast does
! not put the value of an array element actual argument in its place, so
! the loop reads the array and is declined, rather than given device code
! that names an array it does not declare.
module gpu_decline_38_m
    implicit none
    type :: pt
        real :: x
        integer :: k
    end type
    type(pt), parameter :: carr(2) = [pt(1.0, 1), pt(2.0, 2)]
contains
    pure real function f(p, y)
        type(pt), intent(in) :: p
        real, intent(in) :: y
        f = p%x * y + p%k
    end function
end module

program gpu_decline_38
    use gpu_decline_38_m, only: carr, f
    implicit none
    real :: y(20)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = f(carr(2), real(i))
    end do
    do i = 1, size(y)
        if (abs(y(i) - (2.0*i + 2)) > 1e-3) error stop 1
    end do
    print *, y(1:3)
end program
