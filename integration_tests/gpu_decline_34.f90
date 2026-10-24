! gpu decline reason: StructArrayConstant
! a named constant array of a derived type, read in the loop body and by a
! procedure the loop calls, has no value representation in device code.
module gpu_decline_34_m
    implicit none
    type :: pt
        real :: x
        integer :: k
    end type
    type(pt), parameter :: arr(2) = [pt(1.0, 2), pt(3.0, 4)]
contains
    pure real function f(i)
        integer, intent(in) :: i
        f = arr(mod(i, 2) + 1)%x + arr(1)%k
    end function
end module

program gpu_decline_34
    use gpu_decline_34_m, only: arr, f
    implicit none
    real :: q(10), r(10)
    integer :: i
    do concurrent (i = 1:size(q))
        q(i) = arr(mod(i, 2) + 1)%x
        r(i) = f(i)
    end do
    do i = 1, size(q)
        if (abs(q(i) - merge(1.0, 3.0, mod(i, 2) == 0)) > 1e-5) error stop 1
        if (abs(r(i) - (merge(1.0, 3.0, mod(i, 2) == 0) + 2)) > 1e-5) error stop 2
    end do
    print *, q(1:2), r(1:2)
end program
