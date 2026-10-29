! EQUIVALENCE between real and integer arrays, written and read in loops,
! also through an assumed-shape dummy argument associated with one of them.
module tbaa_02_m
implicit none
contains

subroutine pun_local(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    real :: r(8)
    integer :: k(8)
    equivalence (r, k)
    integer :: i
    s = 0
    do i = 1, 8
        r(i) = real(i)
        k(i) = k(i) + n
        if (r(i) /= nearest(real(i), 1.0)) s = s + 1
    end do
end subroutine

subroutine bump(k)
    integer, intent(inout) :: k(:)
    integer :: i
    do i = 1, size(k)
        k(i) = k(i) + 1
    end do
end subroutine

subroutine pun_call(s)
    integer, intent(out) :: s
    real :: r(6)
    integer :: k(6)
    equivalence (r, k)
    integer :: i, j
    s = 0
    do j = 1, 3
        do i = 1, 6
            r(i) = real(i*j)
        end do
        call bump(k)
        do i = 1, 6
            if (r(i) /= nearest(real(i*j), 1.0)) s = s + 1
        end do
    end do
end subroutine

end module

program tbaa_02
use tbaa_02_m
implicit none
integer :: s
call pun_local(1, s)
if (s /= 0) error stop 1
call pun_call(s)
if (s /= 0) error stop 2
print *, "ok"
end program
