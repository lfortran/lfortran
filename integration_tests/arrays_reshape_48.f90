module arrays_reshape_48_mod
implicit none
type :: w
    character(len=2) :: c
    integer, allocatable :: a(:)
end type
type :: wrap
    type(w) :: inner
end type
contains
subroutine assign_assumed_shape(b, src)
    type(w), intent(inout) :: b(:, :)
    type(w), intent(in) :: src(:)
    b = reshape(src, [2, 2])
end subroutine
subroutine assign_explicit_shape(b, src, n)
    integer, intent(in) :: n
    type(w), intent(inout) :: b(n, n)
    type(w), intent(in) :: src(:)
    b = reshape(src, [2, 2])
end subroutine
end module

program arrays_reshape_48
! Assigning `reshape` of an array of structs must free the storage the
! target elements owned before (#13666)
use arrays_reshape_48_mod
implicit none
type(w) :: rs(4), rb(2, 2), rc(3, 2)
type(w), allocatable :: rab(:, :)
type(wrap) :: ws(4), wb(2, 2)
integer :: i, k

do i = 1, 4
    rs(i)%c = achar(96 + i) // 'x'
    allocate(rs(i)%a(2))
    rs(i)%a = [i, 2*i]
    ws(i)%inner%c = rs(i)%c
end do
rc%c = 'qq'
allocate(rab(2, 2))

do k = 1, 3
    rb = reshape(rs, [2, 2])
    if (rb(2, 2)%c /= 'dx' .or. rb(2, 2)%a(2) /= 8) error stop 1

    rab = reshape(rs, [2, 2])
    if (rab(1, 2)%c /= 'cx' .or. rab(1, 2)%a(2) /= 6) error stop 2

    rc(2, :) = reshape(rs(1:2), [2])
    if (rc(2, 2)%c /= 'bx' .or. rc(2, 2)%a(1) /= 2) error stop 3
    if (rc(1, 2)%c /= 'qq' .or. rc(3, 1)%c /= 'qq') error stop 4

    call assign_assumed_shape(rb, rs(4:1:-1))
    if (rb(1, 1)%c /= 'dx' .or. rb(1, 1)%a(1) /= 4) error stop 5
    if (rb(2, 1)%c /= 'cx' .or. rb(1, 2)%a(2) /= 4) error stop 8
    if (rb(2, 2)%c /= 'ax' .or. rb(2, 2)%a(2) /= 2) error stop 9

    call assign_explicit_shape(rb, rs, 2)
    if (rb(1, 1)%c /= 'ax' .or. rb(2, 1)%a(2) /= 4) error stop 6

    wb = reshape(ws, [2, 2])
    if (wb(1, 2)%inner%c /= 'cx') error stop 7
end do

print *, rb%c, rab%c, rc%c, wb%inner%c
end program
