! A do concurrent that calls a pure function with an `error stop` on a branch
! the data never takes. `error stop` is not on the unsupported list: the
! thread records it in the launch's status buffer and the host runs it after
! the launch, so the loop is offloaded on every device, Metal included.
program gpu_unsupported_08
implicit none
integer :: i
real :: a(8)
do concurrent (i = 1:8)
    a(i) = checked_sqrt(real(i))
end do
do i = 1, 8
    if (abs(a(i) - sqrt(real(i))) > 1.0e-5) error stop
end do
print *, "PASS"
contains
    pure real function checked_sqrt(x)
        real, intent(in) :: x
        if (x < 0.0) error stop "negative"
        checked_sqrt = sqrt(x)
    end function
end program
