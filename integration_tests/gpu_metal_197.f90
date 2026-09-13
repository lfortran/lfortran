! Test: an allocatable array allocated inside a do concurrent block that is
! offloaded to the GPU. Its extent is only known at run time, so the device
! cannot declare it; it is backed by a per-thread slice of a workspace
! buffer the host allocates before the launch.
program gpu_metal_197
  implicit none
  integer :: i, n
  real :: r(4)
  n = 3
  r = 0.0
  do concurrent (i = 1:4)
    block
      real, allocatable :: work(:)
      integer :: k
      allocate(work(n))
      do k = 1, n
        work(k) = real(k * i)
      end do
      r(i) = sum(work)
      deallocate(work)
    end block
  end do
  if (abs(r(1) - 6.0) > 1e-6) error stop
  if (abs(r(2) - 12.0) > 1e-6) error stop
  if (abs(r(3) - 18.0) > 1e-6) error stop
  if (abs(r(4) - 24.0) > 1e-6) error stop
  print *, "PASS"
end program
