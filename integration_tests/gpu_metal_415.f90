program gpu_metal_415
  ! The device heap holds everything one launch allocates, over all its
  ! iterations, and it is 256 MiB unless LFORTRAN_GPU_HEAP_SIZE says
  ! otherwise. Here the iterations together allocate more than that, so a
  ! thread finds the heap exhausted and stops, and the program ends after
  ! the launch with "ERROR STOP the device heap is exhausted ...". The test
  ! is registered FAIL, on the GPU backends only: on the CPU every
  ! iteration frees what it allocates, and the program runs to the end.
  implicit none
  integer, parameter :: n = 64
  real :: r(n)
  integer :: i, m(n)
  m = 2000000
  do concurrent (i = 1:n)
    block
      real, allocatable :: w(:)
      integer :: c
      ! A size the iteration counts, which the host cannot work out.
      c = 0
      do while (c < m(i))
        c = c + 1000
      end do
      allocate(w(c))
      w(1) = i
      w(c) = 1
      r(i) = w(1) + w(c) + size(w)
    end block
  end do
  print *, "NOT REACHED", sum(r)
end program
