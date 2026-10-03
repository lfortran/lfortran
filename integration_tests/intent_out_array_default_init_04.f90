! A scalar `intent(out)` dummy of a type with something to finalize is
! finalized on entry, before it becomes undefined (F2018 7.5.6.3), so the
! final subroutine sees the caller's values. F2018 8.5.10 then makes the
! dummy undefined except for its default-initialized subcomponents, which
! include the elements of a derived-type array component: those take the
! component's own default (`c(2) = ta(8)`) or else the default of the element
! type (`h = 5`). This holds whether the type has a final subroutine of its
! own or only a finalizable component, which is finalized with it
! (F2018 7.5.6.2).
module intent_out_array_default_init_04_mod
implicit none

type :: ta
    integer :: h = 5
end type ta

type :: vf
    type(ta) :: c(2)
contains
    final :: fin_vf
end type vf

type :: vg
    type(ta) :: c(2) = ta(8)
contains
    final :: fin_vg
end type vg

type :: fz
    integer :: k = 0
contains
    final :: fin_fz
end type fz

type :: vn
    type(fz) :: q
    type(ta) :: c(2)
    type(ta) :: d(2) = ta(8)
end type vn

integer :: nfin = 0, nfin_old = 0

contains

    subroutine fin_vf(x)
        type(vf), intent(inout) :: x
        nfin = nfin + 1
        if (all(x%c%h == 99)) nfin_old = nfin_old + 1
    end subroutine fin_vf

    subroutine fin_vg(x)
        type(vg), intent(inout) :: x
        nfin = nfin + 1
        if (all(x%c%h == 99)) nfin_old = nfin_old + 1
    end subroutine fin_vg

    subroutine fin_fz(x)
        type(fz), intent(inout) :: x
        nfin = nfin + 1
        if (x%k == 99) nfin_old = nfin_old + 1
    end subroutine fin_fz

    subroutine reset_vf(a)
        type(vf), intent(out) :: a
        if (nfin /= 1 .or. nfin_old /= 1) error stop 1
        if (any(a%c%h /= 5)) error stop 2
    end subroutine reset_vf

    subroutine reset_vg(a)
        type(vg), intent(out) :: a
        if (nfin /= 1 .or. nfin_old /= 1) error stop 3
        if (any(a%c%h /= 8)) error stop 4
    end subroutine reset_vg

    subroutine reset_vn(a)
        type(vn), intent(out) :: a
        if (nfin /= 1 .or. nfin_old /= 1) error stop 5
        if (a%q%k /= 0) error stop 6
        if (any(a%c%h /= 5)) error stop 7
        if (any(a%d%h /= 8)) error stop 8
    end subroutine reset_vn

end module intent_out_array_default_init_04_mod

program intent_out_array_default_init_04
use intent_out_array_default_init_04_mod
implicit none
type(vf) :: f
type(vg) :: g
type(vn) :: n

f%c%h = 99
nfin = 0
nfin_old = 0
call reset_vf(f)
if (any(f%c%h /= 5)) error stop 11

g%c%h = 99
nfin = 0
nfin_old = 0
call reset_vg(g)
if (any(g%c%h /= 8)) error stop 12

n%q%k = 99
n%c%h = 99
n%d%h = 99
nfin = 0
nfin_old = 0
call reset_vn(n)
if (n%q%k /= 0) error stop 13
if (any(n%c%h /= 5)) error stop 14
if (any(n%d%h /= 8)) error stop 15

print *, "ok"
end program intent_out_array_default_init_04
