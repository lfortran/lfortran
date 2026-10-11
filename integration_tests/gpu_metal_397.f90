module gpu_metal_397_m
  implicit none
contains
  pure function total(x, k) result(s)
    real, intent(in) :: x(:)
    integer, intent(in) :: k
    real :: s
    real, allocatable :: w(:,:)
    integer :: i, j
    allocate(w(size(x),2))
    do j = 1, 2
      do i = 1, size(x)
        w(i,j) = x(i) + 10*j + k
      end do
    end do
    s = sum(w)
  end function

  pure function weighted(a, k) result(s)
    real, intent(in) :: a(:)
    integer, intent(in) :: k
    real :: s
    real :: b(0:2)
    b = [1.0, 2.0, real(k)]
    s = sum(a(2:4) * b)
  end function

  pure function offset_sum(k) result(s)
    integer, intent(in) :: k
    real :: s
    real :: c(0:2, -1:0)
    c(:, -1) = [1.0, 2.0, 3.0]
    c(:, 0) = [4.0, 5.0, real(k)]
    s = sum(c)
  end function

end module

program gpu_metal_397
  use gpu_metal_397_m
  implicit none
  real :: s1(3), s2(3), s3(3), a(4), b0(0:2)
  logical :: r(3)
  integer :: i
  a = [1.0, 2.0, 3.0, 4.0]
  b0 = [2.0, 3.0, 4.0]
  do concurrent (i = 1:3)
    s1(i) = 1 + total(a, i)
    s2(i) = 1 + weighted(a, i)
    s3(i) = 1 + offset_sum(i)
    r(i) = all(a(2:4) == b0 + real(i - 3))
  end do
  print *, s1, s2, s3, r
  do i = 1, 3
    if (abs(s1(i) - (141 + 8*i)) > 1e-4) error stop 1
    if (abs(s2(i) - (1 + 8 + 4*i)) > 1e-4) error stop 2
    if (abs(s3(i) - (1 + 15 + i)) > 1e-4) error stop 3
    if (r(i) .neqv. (i == 3)) error stop 4
  end do
end program
