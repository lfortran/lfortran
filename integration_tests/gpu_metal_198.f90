! Test: an automatic array whose extent is a run-time value, declared in a
! block inside a do concurrent that is offloaded to the GPU.
program gpu_metal_198
  implicit none
  integer :: i, n
  real :: r(4)
  n = 3
  r = 0.0
  do concurrent (i = 1:4)
    block
      real :: work(n)
      integer :: k
      do k = 1, n
        work(k) = real(k * i)
      end do
      r(i) = sum(work)
    end block
  end do
  if (abs(r(1) - 6.0) > 1e-6) error stop
  if (abs(r(2) - 12.0) > 1e-6) error stop
  if (abs(r(3) - 18.0) > 1e-6) error stop
  if (abs(r(4) - 24.0) > 1e-6) error stop
  print *, "PASS"
end program
