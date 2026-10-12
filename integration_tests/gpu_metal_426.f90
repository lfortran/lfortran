module gpu_metal_426_m
  ! A section that is not contiguous, passed for an assumed-shape dummy
  ! that a parallel loop reads, is copied into a contiguous array the launch
  ! hands the kernel, with a size that asks whether that array is allocated.
  ! With --fast the array stays allocatable.
  implicit none
contains
  subroutine copy_dc(vec, r, n)
    real, intent(in) :: vec(:)
    integer, intent(in) :: n
    real, intent(out) :: r(n)
    integer :: j
    do concurrent (j = 1:n)
      r(j) = vec(j)
    end do
  end subroutine
end module

program gpu_metal_426
  use gpu_metal_426_m
  implicit none
  real :: c(4, 3, 2), r(3)
  integer :: i, j, k
  do k = 1, 2
    do j = 1, 3
      do i = 1, 4
        c(i, j, k) = i + 10*j + 100*k
      end do
    end do
  end do
  call copy_dc(c(2, :, 2), r, 3)
  print *, r
  if (any(r /= [212., 222., 232.])) error stop
end program
