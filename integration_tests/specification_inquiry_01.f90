! The bounds and lengths of dummies and function results can inquire about
! local objects (F2018 10.1.11): module procedures (in a module compiled
! separately, specification_inquiry_01b.f90) and a separate module
! procedure of a submodule, called directly, through a procedure pointer
! and through a dummy procedure.
!
! gfortran evaluates the bounds of a dummy before it allocates the automatic
! objects that they inquire about, so `s` is called only with other
! compilers (`strict`).
module specification_inquiry_01_s
implicit none
interface
    module function h(n) result(r)
    integer, intent(in) :: n
    integer :: w(0:n)
    integer :: r(ubound(w, 1))
    end function
end interface
end module

submodule (specification_inquiry_01_s) specification_inquiry_01_sub
implicit none
contains
    module function h(n) result(r)
    integer, intent(in) :: n
    integer :: w(0:n)
    integer :: r(ubound(w, 1))
    r = 3
    end function
end submodule

program specification_inquiry_01
use iso_fortran_env, only: compiler_version
use specification_inquiry_01_m, only: f, g, s
use specification_inquiry_01_s, only: h
implicit none
procedure(f), pointer :: p
procedure(s), pointer :: q
integer, allocatable :: z(:)
character(len=:), allocatable :: cc(:)
real :: a(6)
integer :: k
logical :: strict
strict = index(compiler_version(), "GCC") == 0

allocate(z(5))
z = f(3)
if (size(z) /= 5 .or. any(z /= 7)) error stop 1
if (sum(f(1)) /= 21) error stop 2
p => f
if (size(p(4)) /= 6) error stop 3
call run_f(f)

allocate(character(len=5) :: cc(6))
cc = g(4)
if (size(cc) /= 6 .or. len(cc) /= 5) error stop 5
if (any(cc /= "x")) error stop 6

deallocate(z)
allocate(z(4))
z = h(4)
if (size(z) /= 4 .or. any(z /= 3)) error stop 7
if (size(h(0)) /= 0) error stop 8

if (strict) then
    a = 0
    call s(a(1:3), 3, k)
    if (k /= 3) error stop 9
    call s(a(2:6:2), 3, k)
    q => s
    call q(a, 3, k)
    call run_s(s)
    if (any(a /= [3, 4, 3, 1, 0, 1])) error stop 10
end if
print *, size(z), len(cc), a
contains
    subroutine run_f(e)
    procedure(f) :: e
    if (size(e(2)) /= 4 .or. any(e(2) /= 7)) error stop 4
    end subroutine
    subroutine run_s(e)
    procedure(s) :: e
    call e(a, 3, k)
    end subroutine
end program
