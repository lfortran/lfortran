program gpu_metal_447
  ! Per-thread workspaces of loops with fewer iterations than a block of
  ! threads, and of a loop that ends part way into its second block. The
  ! host gives a workspace one slice per iteration, not per thread it
  ! starts, as large as the largest iteration needs: here the last one.
  implicit none
  integer, parameter :: big = 300
  integer :: n, i, j, m(big)
  real :: total(3), last(3), sums(big), expected(big)

  n = 5
  m(1:3) = [2, 7, 4]
  do concurrent (i = 1:3)
    block
      real :: work(n)
      real, allocatable :: scaled(:)
      allocate(scaled(m(i)))
      do j = 1, n
        work(j) = i * j
      end do
      do j = 1, m(i)
        scaled(j) = work(mod(j - 1, n) + 1) * 2
      end do
      total(i) = sum(work) + sum(scaled)
      last(i) = scaled(m(i))
    end block
  end do
  if (any(total /= [21., 102., 105.])) error stop 1
  if (any(last /= [4., 8., 24.])) error stop 2

  do i = 1, big
    m(i) = i / 10 + 1
  end do
  do concurrent (i = 1:big)
    block
      real, allocatable :: w(:)
      allocate(w(m(i)))
      do j = 1, m(i)
        w(j) = i + j
      end do
      sums(i) = sum(w)
    end block
  end do
  do i = 1, big
    expected(i) = m(i) * i + m(i) * (m(i) + 1) / 2
  end do
  if (any(sums /= expected)) error stop 3
  print *, "ok"
end program
