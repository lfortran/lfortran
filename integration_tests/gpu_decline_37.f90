! gpu decline reason: StructArrayConstant
! a component of an element of a named constant array of a derived type,
! read in a subscript of the assignment target in the loop body. Checked
! with --fast: --fast does not put values in an assignment target, so
! the loop reads the array and is declined, rather than given device code
! that names an array it does not declare.
module gpu_decline_37_m
    implicit none
    type :: pt
        real :: x
        integer :: k
    end type
    type(pt), parameter :: carr(2) = [pt(1.0, 1), pt(2.0, 2)]
end module

program gpu_decline_37
    use gpu_decline_37_m, only: carr
    implicit none
    real :: y(22)
    integer :: i
    y = 0
    do concurrent (i = 1:20)
        y(i + carr(2)%k) = i
    end do
    do i = 1, 20
        if (abs(y(i + 2) - i) > 1e-3) error stop 1
    end do
    print *, y(1:4)
end program
