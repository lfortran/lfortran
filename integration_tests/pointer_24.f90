module pointer_24_mod
implicit none

type :: t
    integer :: x = 0
end type

contains

    logical function assoc_int(p, d) result(r)
    integer, pointer, intent(in) :: p(:)
    integer, target, intent(in) :: d(:)
    r = associated(p, d)
    end function

    logical function assoc_real_2d(p, d) result(r)
    real(8), pointer, intent(in) :: p(:, :)
    real(8), target, intent(in) :: d(:, :)
    r = associated(p, d)
    end function

    logical function assoc_class(p, d) result(r)
    class(t), pointer, intent(in) :: p(:)
    class(t), target, intent(in) :: d(:)
    r = associated(p, d)
    end function

    logical function assoc_type(p, d) result(r)
    type(t), pointer, intent(in) :: p(:)
    type(t), target, intent(in) :: d(:)
    r = associated(p, d)
    end function

    logical function assoc_contiguous(p, d) result(r)
    integer, pointer, intent(in) :: p(:)
    integer, target, contiguous, intent(in) :: d(:)
    r = associated(p, d)
    end function

    logical function assoc_char(p, d) result(r)
    character(len=4), pointer, intent(in) :: p(:)
    character(len=4), target, intent(in) :: d(:)
    r = associated(p, d)
    end function

    logical function assoc_host(p, d) result(r)
    integer, pointer, intent(in) :: p(:)
    integer, target, intent(in) :: d(:)
    r = inner()
    contains
        logical function inner()
        inner = associated(p, d)
        end function
    end function

end module

program pointer_24
use pointer_24_mod
implicit none

integer, target :: a(3), b(3), s(6)
integer, pointer :: p(:)
real(8), target :: ra(2, 3), rb(2, 3)
real(8), pointer :: rp(:, :)
type(t), target :: ta(3), tb(3)
class(t), pointer :: cp(:)
type(t), pointer :: tp(:)
character(len=4), target :: ca(3), cb(3)
character(len=4), pointer :: chp(:)

p => a
if (.not. assoc_int(p, a)) error stop 1
if (assoc_int(p, b)) error stop 2
nullify(p)
if (assoc_int(p, a)) error stop 3

rp => ra
if (.not. assoc_real_2d(rp, ra)) error stop 4
if (assoc_real_2d(rp, rb)) error stop 5

cp => ta
if (.not. assoc_class(cp, ta)) error stop 6
if (assoc_class(cp, tb)) error stop 7

tp => ta
if (.not. assoc_type(tp, ta)) error stop 8
if (assoc_type(tp, tb)) error stop 9

p => a
if (.not. assoc_contiguous(p, a)) error stop 10
if (assoc_contiguous(p, b)) error stop 11

if (.not. assoc_host(p, a)) error stop 12
if (assoc_host(p, b)) error stop 13

p => s(1:5:2)
if (.not. assoc_int(p, s(1:5:2))) error stop 14
if (assoc_int(p, s(2:6:2))) error stop 15

chp => ca
if (.not. assoc_char(chp, ca)) error stop 16
if (assoc_char(chp, cb)) error stop 17

print *, "ok"
end program
