! equivalent of openmp_01.f90

subroutine omp_func(n)
implicit none
integer, intent(in) :: n
integer :: i

do concurrent (i = 1:n) shared(n)
    print *, "xyz"
end do

print *, "n = ", n
if (n /= 100) error stop
end subroutine

program do_concurrent_02
integer, parameter :: n = 100
call omp_func(n)
print *, "Done for n = ", n
end program
