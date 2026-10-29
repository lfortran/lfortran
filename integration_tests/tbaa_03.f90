! A COMMON block with different layouts in different procedures, written
! and read in loops.
subroutine tbaa_03_fill(n)
    implicit none
    integer, intent(in) :: n
    real :: a(4)
    integer :: m(4)
    common /tbaa_03_blk/ a, m
    integer :: i
    do i = 1, 4
        a(i) = real(i*n)
        m(i) = i + n
        a(i) = a(i) + m(i)
    end do
end subroutine

subroutine tbaa_03_check(n, s)
    implicit none
    integer, intent(in) :: n
    integer, intent(out) :: s
    integer :: ia(4)
    real :: x(2), y(2)
    integer :: m(4)
    common /tbaa_03_blk/ ia, m
    equivalence (x, m(1)), (y, m(3))
    integer :: i
    s = 0
    do i = 1, 4
        if (ia(i) /= transfer(real(i*n + i + n), 0)) s = s + 1
        ia(i) = ia(i) + 1
    end do
    x(1) = 1.5
    if (m(1) /= transfer(1.5, 0)) s = s + 10
    y(2) = -2.5
    if (m(4) /= transfer(-2.5, 0)) s = s + 100
end subroutine

subroutine tbaa_03_reread(n, s)
    implicit none
    integer, intent(in) :: n
    integer, intent(inout) :: s
    real :: a(4)
    integer :: m(4)
    common /tbaa_03_blk/ a, m
    integer :: i
    do i = 1, 4
        if (a(i) /= nearest(real(i*n + i + n), 1.0)) s = s + 1000
        a(i) = 0
    end do
end subroutine

program tbaa_03
implicit none
integer :: s
call tbaa_03_fill(3)
call tbaa_03_check(3, s)
call tbaa_03_reread(3, s)
print *, s
if (s /= 0) error stop 1
print *, "ok"
end program
