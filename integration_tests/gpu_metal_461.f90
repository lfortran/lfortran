program gpu_metal_461
  ! Test: a kernel launched in a host loop stops at the seventh launch, and
  ! the launches do not wait for it. The program has to end at that stop,
  ! with its code, before anything after the loop runs, as it does on the
  ! CPU. The test is registered FAIL: it checks that the program stops
  ! rather than reaching the print below.
  implicit none
  integer, parameter :: n = 64
  real :: a(n)
  integer :: i, k
  a = 0
  do k = 1, 20
    do concurrent (i = 1:n)
      if (a(i) > 6.5) error stop 7
      a(i) = a(i) + 1
    end do
  end do
  print *, "NOT REACHED", sum(a)
end program
