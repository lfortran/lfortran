! A non-contiguous actual argument passed to a CONTIGUOUS assumed-shape or
! assumed-rank dummy is copied in to a contiguous temporary, and copied back
! unless the dummy is intent(in) (#17133).
module contiguous_dummy_01_m
use iso_c_binding, only: c_float, c_loc, c_f_pointer
implicit none
contains

! Explicit-shape: relies on getting contiguous memory.
subroutine es(x)
real :: x(3)
x = x * 10
end subroutine

subroutine es_sum(x, s)
real, intent(in) :: x(3)
real, intent(out) :: s
s = x(1) + 2*x(2) + 4*x(3)
end subroutine

subroutine es_set(x)
real, intent(out) :: x(3)
x = [7, 8, 9]
end subroutine

subroutine cs(b)
real, contiguous :: b(:)
if (.not. is_contiguous(b)) error stop "cs: not contiguous"
call es(b)
end subroutine

subroutine csi(b)
real, contiguous, intent(inout) :: b(:)
if (.not. is_contiguous(b)) error stop "csi: not contiguous"
call es(b)
end subroutine

subroutine csin(b, s)
real, contiguous, intent(in) :: b(:)
real, intent(out) :: s
if (.not. is_contiguous(b)) error stop "csin: not contiguous"
call es_sum(b, s)
end subroutine

subroutine cso(b)
real, contiguous, intent(out) :: b(:)
call es_set(b)
end subroutine

subroutine cs2(b)
real, contiguous :: b(:,:)
if (.not. is_contiguous(b)) error stop "cs2: not contiguous"
call es(b)
end subroutine

subroutine cr(b)
real, contiguous :: b(..)
if (.not. is_contiguous(b)) error stop "cr: not contiguous"
select rank (b)
rank (1)
    call es(b)
end select
end subroutine

subroutine cri(b)
real, contiguous, intent(inout) :: b(..)
select rank (b)
rank (1)
    call es(b)
end select
end subroutine

subroutine crin(b, s)
real, contiguous, intent(in) :: b(..)
real, intent(out) :: s
select rank (b)
rank (1)
    call es_sum(b, s)
end select
end subroutine

! The dummy of the issue: type(*), assumed-rank, accessed through c_loc.
subroutine tr_read(buf, out)
type(*), dimension(..), contiguous, target, intent(in) :: buf
real(8), intent(out) :: out(3)
real(8), pointer :: p(:)
call c_f_pointer(c_loc(buf), p, [3])
out = p
end subroutine

subroutine tr_write(buf)
type(*), dimension(..), contiguous, target, intent(inout) :: buf
real(8), pointer :: p(:)
call c_f_pointer(c_loc(buf), p, [3])
p = [10, 20, 30]
end subroutine

! A bind(c) procedure gets the contiguous temporary as well.
subroutine cb(b) bind(c)
real(c_float), contiguous :: b(:)
if (.not. is_contiguous(b)) error stop "cb: not contiguous"
call es(b)
end subroutine

! A non-contiguous assumed-shape dummy forwarded to a contiguous one.
subroutine fwd(a)
real :: a(:)
call cs(a)
end subroutine

! A non-contiguous assumed-rank dummy forwarded to a contiguous one.
subroutine fwd_rank(a)
real :: a(..)
select rank (a)
rank (1)
    call cs(a)
end select
end subroutine

end module

program contiguous_dummy_01
use contiguous_dummy_01_m
implicit none
real, target :: d(6), m(2, 3)
real, pointer :: q(:)
real(8) :: d8(6), out8(3)
real :: s

d = [1, 2, 3, 4, 5, 6]
call cs(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 1

d = [1, 2, 3, 4, 5, 6]
call csi(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 2

d = [1, 2, 3, 4, 5, 6]
call csin(d(2::2), s)
if (abs(s - (2 + 2*4 + 4*6)) > 1e-6) error stop 3
if (any(d /= [1, 2, 3, 4, 5, 6])) error stop 4

d = [1, 2, 3, 4, 5, 6]
call cso(d(1::2))
if (any(d /= [7, 2, 8, 4, 9, 6])) error stop 5

m = reshape([1, 2, 3, 4, 5, 6], [2, 3])
call cs2(m(2:2, :))
if (any(m /= reshape([1, 20, 3, 40, 5, 60], [2, 3]))) error stop 6

d = [1, 2, 3, 4, 5, 6]
call cr(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 7

d = [1, 2, 3, 4, 5, 6]
call cri(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 8

d = [1, 2, 3, 4, 5, 6]
call crin(d(2::2), s)
if (abs(s - (2 + 2*4 + 4*6)) > 1e-6) error stop 9
if (any(d /= [1, 2, 3, 4, 5, 6])) error stop 10

d = [1, 2, 3, 4, 5, 6]
q => d(2::2)
call cs(q)
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 11

d = [1, 2, 3, 4, 5, 6]
call fwd(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 12

! Contiguous actuals are passed as they are.
d = [1, 2, 3, 4, 5, 6]
call cs(d(2:4))
if (any(d /= [1, 20, 30, 40, 5, 6])) error stop 13

d8 = [1, 2, 3, 4, 5, 6]
call tr_read(d8(2::2), out8)
if (any(out8 /= [2, 4, 6])) error stop 14
call tr_write(d8(1::2))
if (any(d8 /= [10, 2, 20, 4, 30, 6])) error stop 15

d = [1, 2, 3, 4, 5, 6]
call fwd_rank(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 16

d = [1, 2, 3, 4, 5, 6]
call cb(d(2::2))
if (any(d /= [1, 20, 3, 40, 5, 60])) error stop 17

print *, "ok"
end program
