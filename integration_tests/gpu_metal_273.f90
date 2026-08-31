! A run-time sized temporary for an overlapping array assignment inside a
! `do concurrent`.
!
! `ap(:,c) = ap(n:1:-1,c)` reads storage the same statement writes, so it
! needs a temporary. When the temporary cannot be given a compile-time
! extent it has to be per-thread: one temporary shared by every thread of
! the kernel would let one iteration overwrite another's working copy. The
! iterations below all carry different values, so a shared temporary shows
! up as one column holding another column's numbers.
module gpu_metal_273_m
implicit none
contains

  ! Reverse each column in place. `a` is assumed-shape, so the temporary
  ! is sized only at run time.
  subroutine reverse_columns(a)
    real, intent(inout) :: a(:,:)
    integer :: c
    do concurrent (c = 1:size(a,2))
      a(:,c) = a(size(a,1):1:-1,c)
    end do
  end subroutine

  ! The same shape with a compile-time extent, as a fence: this one keeps
  ! its temporary on the kernel stack.
  subroutine reverse_rows_fixed(a)
    real, intent(inout) :: a(8,12)
    integer :: r
    do concurrent (r = 1:12)
      a(:,r) = a(8:1:-1,r)
    end do
  end subroutine

  ! An array-valued ASSOCIATE selector over an assumed-shape dummy: the
  ! frontend materialises `w` as a run-time sized temporary of the
  ! ASSOCIATE construct, which also has to be per-thread.
  subroutine scale_columns(x, y, g)
    real, intent(in) :: x(:), y(:)
    real, intent(out) :: g(:,:)
    integer :: j
    do concurrent (j = 1:size(y))
      associate (w => x + 1000.0*y(j))
        g(:,j) = 2.0*w
      end associate
    end do
  end subroutine

end module

program gpu_metal_273
use gpu_metal_273_m
implicit none
integer, parameter :: n = 9, m = 24
real :: a(n,m), fixed(8,12), x(n), y(m), g(n,m)
integer :: i, j

! Run-time sized alias temporary.
do j = 1, m
  do i = 1, n
    a(i,j) = real(100*j + i)
  end do
end do
call reverse_columns(a)
do j = 1, m
  do i = 1, n
    if (abs(a(i,j) - real(100*j + (n-i+1))) > 0.0) error stop "reverse_columns"
  end do
end do

! Fixed-size alias temporary (fence).
do j = 1, 12
  do i = 1, 8
    fixed(i,j) = real(100*j + i)
  end do
end do
call reverse_rows_fixed(fixed)
do j = 1, 12
  do i = 1, 8
    if (abs(fixed(i,j) - real(100*j + (8-i+1))) > 0.0) error stop "reverse_rows_fixed"
  end do
end do

! Run-time sized ASSOCIATE temporary.
do i = 1, n
  x(i) = real(i)
end do
do j = 1, m
  y(j) = real(j)
end do
g = 0.0
call scale_columns(x, y, g)
do j = 1, m
  do i = 1, n
    if (abs(g(i,j) - 2.0*(real(i) + 1000.0*real(j))) > 0.0) error stop "scale_columns"
  end do
end do

print *, "ok"
end program
