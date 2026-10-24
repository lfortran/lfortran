program gpu_metal_441
  ! An associate name bound to an expression is the value the expression had
  ! when the construct was entered: the kernel is handed that value, not
  ! the expression to evaluate again, nor the arrays the expression reads.
  implicit none
  type t
    real, allocatable :: v(:)
  end type
  type(t), allocatable :: big(:)
  integer :: k, l, i
  real :: w(4)
  allocate(big(1000))
  do i = 1, size(big)
    big(i)%v = [real(i)]
  end do
  k = 2
  w = 0
  associate(m => k + 1, n => size(big))
    k = 5
    do i = 1, 3
      do concurrent (l = 1:size(w))
        w(l) = w(l) + m + real(l) / n
      end do
    end do
  end associate
  print *, w
  do l = 1, size(w)
    if (abs(w(l) - 3 * (3 + real(l) / 1000)) > 1e-4) error stop
  end do
end program
