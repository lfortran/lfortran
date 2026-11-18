subroutine read_unspecified(m, n, p)
! No intent: the scalar is shared and only read inside do concurrent.
integer, intent(in) :: n
integer :: m
integer :: p(n)
integer :: i
do concurrent (i = 1:n)
    p(i) = m
end do
end subroutine read_unspecified

subroutine read_inout(m, n, p)
integer, intent(in) :: n
integer, intent(inout) :: m
integer :: p(n)
integer :: i
do concurrent (i = 1:n)
    p(i) = m
end do
end subroutine read_inout

subroutine read_in(m, n, p)
! intent(in) is private. This must keep compiling and keep the value.
integer, intent(in) :: n, m
integer :: p(n)
integer :: i
do concurrent (i = 1:n)
    p(i) = m
end do
end subroutine read_in

subroutine write_inout(m)
integer, intent(inout) :: m
integer :: i
do concurrent (i = 1:1)
    m = 11
end do
end subroutine write_inout

subroutine write_out(m)
integer, intent(out) :: m
integer :: i
do concurrent (i = 1:1)
    m = 13
end do
end subroutine write_out

program openmp_119
implicit none
integer :: a(2), b(2), c(2)
integer :: m, k, q(2), i

call read_unspecified(7, 2, a)
if (a(1) /= 7 .or. a(2) /= 7) error stop 1

m = 7
call read_inout(m, 2, b)
if (b(1) /= 7 .or. b(2) /= 7) error stop 2
if (m /= 7) error stop 3

call read_in(7, 2, c)
if (c(1) /= 7 .or. c(2) /= 7) error stop 4

k = 5
do concurrent (i = 1:2)
    q(i) = k
end do
if (q(1) /= 5 .or. q(2) /= 5) error stop 5

k = 1
do concurrent (i = 1:1)
    k = 9
end do
if (k /= 9) error stop 6

m = 3
call write_inout(m)
if (m /= 11) error stop 7

call write_out(m)
if (m /= 13) error stop 8
end program openmp_119
