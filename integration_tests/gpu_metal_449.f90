program gpu_metal_449
  ! A `do concurrent` nest with an empty index range runs no iteration,
  ! however many of its ranges run backwards: the number of iterations
  ! is the product of the extents, each of them at least zero.
  implicit none
  integer :: i, j, k, n, m, lo
  real :: a(10, 10), b(10, 10, 10)
  integer :: c(-5:5, -5:5)
  n = 1
  m = 1
  lo = 5

  a = 0
  do concurrent (i = lo:n, j = lo:m)
    a(i, j) = 1
  end do
  if (any(a /= 0)) error stop 1

  ! A single empty range empties the nest as well.
  a = 0
  do concurrent (i = 1:3, j = lo:m)
    a(i, j) = 2
  end do
  if (any(a /= 0)) error stop 2

  a = 0
  do concurrent (i = lo:n, j = 1:3)
    a(i, j) = 3
  end do
  if (any(a /= 0)) error stop 3

  ! Lower bounds other than one.
  c = 0
  do concurrent (i = lo-3:n-2, j = lo-6:m-4)
    c(i, j) = 1
  end do
  if (any(c /= 0)) error stop 4

  b = 0
  do concurrent (i = lo:n, j = 1:2, k = lo:m)
    b(i, j, k) = 1
  end do
  if (any(b /= 0)) error stop 5

  b = 0
  do concurrent (i = lo:n, j = lo:m, k = lo:n)
    b(i, j, k) = 1
  end do
  if (any(b /= 0)) error stop 6

  ! A range running far backwards.
  a = 0
  do concurrent (i = 1000*lo:n, j = 1:2)
    a(j, j) = 4
  end do
  if (any(a /= 0)) error stop 7

  ! A nest that is not empty still runs every iteration once.
  a = 0
  do concurrent (i = n+1:lo-1, j = m+2:lo)
    a(i, j) = a(i, j) + 1
  end do
  if (sum(a) /= 9) error stop 8
  if (any(a(2:4, 3:5) /= 1)) error stop 9
  print *, sum(a), sum(b), sum(c)
end program
