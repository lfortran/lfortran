! gpu decline reason: StructConstantComponent
! a procedure the loop body calls reads a derived-type named constant with
! a complex component, which the device struct cannot be given a value of.
module gpu_decline_27_m
    implicit none
    type :: u
        complex :: z
        integer :: k
    end type
    type(u), parameter :: q = u((1.0, 2.0), 3)
contains
    pure real function fq(x)
        real, intent(in) :: x
        fq = x * aimag(q%z) + q%k
    end function
end module

program gpu_decline_27
    use gpu_decline_27_m, only: fq
    implicit none
    real :: o(4)
    integer :: i
    do concurrent (i = 1:size(o))
        o(i) = fq(real(i))
    end do
    if (abs(o(4) - 11) > 1e-5) error stop "bad o(4)"
    if (abs(o(1) - 5) > 1e-5) error stop "bad o(1)"
end program
