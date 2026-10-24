! gpu decline reason: StructArrayConstant
! a component of an element of a named constant array of a derived type,
! read in an array subscript in the loop body. Checked with --fast:
! --fast does not put the value of a designator inside a subscript in its
! place, so the loop reads the array and is declined, rather than given
! device code that names an array it does not declare.
module gpu_decline_36_m
    implicit none
    type :: pt
        real :: x
        integer :: k
    end type
    type(pt), parameter :: carr(2) = [pt(1.0, 1), pt(2.0, 2)]
end module

program gpu_decline_36
    use gpu_decline_36_m, only: carr
    implicit none
    real :: x(22), y(20)
    integer :: i
    do i = 1, size(x)
        x(i) = i
    end do
    do concurrent (i = 1:size(y))
        y(i) = x(i + carr(2)%k)
    end do
    do i = 1, size(y)
        if (abs(y(i) - (i + 2)) > 1e-3) error stop 1
    end do
    print *, y(1:3)
end program
