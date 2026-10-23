! gpu decline reason: StructArrayConstant
! a named constant array of a derived type built by an implied-do
! constructor, which has no compile-time value, read in the loop body.
! Registered for gfortran only: LFortran cannot compile the constant on
! the CPU yet (#15936).
module gpu_decline_35_m
    implicit none
    type :: pt
        real :: x
        integer :: k
    end type
    integer :: j
    type(pt), parameter :: arr(3) = [(pt(real(j), j), j = 1, 3)]
end module

program gpu_decline_35
    use gpu_decline_35_m, only: arr
    implicit none
    real :: t(9)
    integer :: i
    do concurrent (i = 1:size(t))
        t(i) = arr(mod(i, 3) + 1)%x * i
    end do
    do i = 1, size(t)
        if (abs(t(i) - real(mod(i, 3) + 1) * i) > 1e-3) error stop 1
    end do
    print *, t(1:3)
end program
