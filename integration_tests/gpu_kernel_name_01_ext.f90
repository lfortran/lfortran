! An external subroutine with a kernel of its own, compiled into an object
! file of its own and linked with the main program.
subroutine gpu_kernel_name_01_ext(n, b)
implicit none
integer, intent(in) :: n
integer, intent(out) :: b(n)
integer :: i
do concurrent (i = 1:n)
    b(i) = 2 * i
end do
end subroutine
