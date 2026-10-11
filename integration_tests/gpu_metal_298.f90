program gpu_metal_298
  ! Test: loops the offload pass rewrites -- by splicing a callee with a
  ! run-time sized local into the kernel as a `__gpu_inl_*` Block, or by
  ! inlining a bare ASSOCIATE -- next to an `error stop` they never reach.
  !
  ! The `error stop` once kept these loops off the device after the rewrite,
  ! which then had to be given back. It is now lowered into a write to the
  ! launch's status buffer, so the loops are offloaded with the rewrite in
  ! place, and the inliner must still rewrite a copy of the ASSOCIATE the
  ! host nest names rather than the original. Every loop has to compute
  ! what Fortran says it computes.
  implicit none

  integer, parameter :: n = 4
  integer :: a(n), b(n), c(n), d(n), e(n), i

  a = 0
  b = 0
  c = 0
  d = 0
  e = 0

  do concurrent (i = 1:n)
    a(i) = twice_sum(n)
    if (i < 0) error stop "unreachable"
  end do

  if (sum(a) /= n * (2 * n)) error stop "spliced then declined"
  if (a(1) /= 2 * n) error stop "spliced then declined first"

  do concurrent (i = 1:n)
    associate (t => i * 2)
      b(i) = t
    end associate
    if (i < 0) error stop "unreachable"
  end do

  if (sum(b) /= 20) error stop "associate then declined"
  if (b(1) /= 2) error stop "associate then declined first"
  if (b(4) /= 8) error stop "associate then declined last"

  ! Nested ASSOCIATE inside BLOCK: the inner construct lives in the
  ! outer ASSOCIATE's table, not the BLOCK's. A copy that only retargets
  ! through the BLOCK table still inlines the host ASSOCIATE.
  do concurrent (i = 1:n)
    block
      associate (t => [i, i + 1])
        d(i) = t(1) + t(2)
      end associate
    end block
    if (i < 0) error stop "unreachable"
  end do

  if (sum(d) /= 24) error stop "associate in block then declined"
  if (d(1) /= 3) error stop "associate in block then declined first"

  ! Three-level ASSOCIATE: each inner call has to be retargeted through
  ! its parent's table, not the outermost one.
  do concurrent (i = 1:n)
    associate (x => i)
      associate (y => x + 1)
        associate (z => y + 1)
          e(i) = z
        end associate
      end associate
    end associate
    if (i < 0) error stop "unreachable"
  end do

  if (sum(e) /= 18) error stop "nested associate then declined"
  if (e(1) /= 3) error stop "nested associate then declined first"

  ! Offloaded, and the first kernel actually emitted.
  do concurrent (i = 1:n)
    c(i) = a(i) + b(i)
  end do

  if (sum(c) /= sum(a) + sum(b)) error stop "offloaded loop"

  print *, "PASS"

contains

  pure function twice_sum(m) result(s)
    integer, intent(in) :: m
    integer :: s
    integer :: tmp(m), k
    do k = 1, m
      tmp(k) = 2
    end do
    s = sum(tmp)
  end function

end program gpu_metal_298
