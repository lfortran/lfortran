program gpu_metal_464
  ! Kernels launched back to back, none of them waited for, each depending
  ! on the ones before it: one reads what the previous one wrote, one
  ! writes what the previous one read, two write the same array, and some
  ! are independent of each other. The device may run independent launches
  ! together, but has to give every launch the results of the launches
  ! before it that it depends on.
  implicit none
  integer, parameter :: n = 1000, steps = 2000
  integer :: a(n), b(n), c(n), d(n), e(n)
  integer :: i, k
  a = 0; b = 0; c = 0; d = 0; e = 0
  do k = 1, steps
    do concurrent (i = 1:n)
      a(i) = a(i) + 1
    end do
    ! Reads what the launch before wrote.
    do concurrent (i = 1:n)
      b(i) = a(i) + i
    end do
    ! Independent of the two before.
    do concurrent (i = 1:n)
      d(i) = d(i) + 2
    end do
    ! Writes what the launch before the last read.
    do concurrent (i = 1:n)
      a(i) = a(i) * 2 - a(i)
    end do
    ! Writes what another launch writes, then reads it back.
    do concurrent (i = 1:n)
      c(i) = b(i)
    end do
    do concurrent (i = 1:n)
      c(i) = c(i) - i + d(i) - 2 * k
    end do
    do concurrent (i = 1:n)
      e(i) = e(i) + c(i) - a(i)
    end do
  end do
  if (any(a /= steps)) error stop 1
  if (any(b /= [(steps + i, i = 1, n)])) error stop 2
  if (any(c /= steps)) error stop 3
  if (any(d /= 2 * steps)) error stop 4
  if (any(e /= 0)) error stop 5
  print *, "ok"
end program
