! An allocatable scalar function result assigned to a variable that is not
! allocatable (issue #13594)
module functions_65_mod
implicit none

type :: w
    integer :: c
end type

type :: wa
    integer :: c(3)
end type

type :: ws
    character(len=4) :: c(3)
end type

type :: wd
    integer, allocatable :: x(:)
    character(len=:), allocatable :: s
end type

type :: holder
    type(wd) :: in
end type

integer :: nfin = 0
integer :: fin_c(2) = -1

type :: tf
    integer :: c
contains
    final :: tf_final
end type

contains

subroutine tf_final(x)
    type(tf), intent(inout) :: x
    nfin = nfin + 1
    if (nfin <= 2) fin_c(nfin) = x%c
end subroutine

function mk_mod() result(r)
    type(w), allocatable :: r
    allocate(r)
    r%c = 4
end function

function mk_wd(k) result(r)
    integer, intent(in) :: k
    type(wd), allocatable :: r
    allocate(r)
    r%x = [k, 2*k]
    r%s = repeat("z", k)
end function

function mk_class() result(r)
    class(w), allocatable :: r
    allocate(r)
    r%c = 9
end function

function mk_tf() result(r)
    type(tf), allocatable :: r
    allocate(r)
    r%c = 5
end function

end module

program functions_65
use functions_65_mod
implicit none
type(w) :: s
type(wa) :: sa
type(ws) :: ss
type(wd) :: d, dv(3)
type(holder) :: h
type(w), pointer :: p
type(w), target :: t
type(tf) :: g
integer :: i

s = mk1()
if (s%c /= 3) error stop 1

s = mk_mod()
if (s%c /= 4) error stop 2

sa = mk_arr()
if (any(sa%c /= [1, 2, 3])) error stop 3

ss = mk_str()
if (any(ss%c /= ["abcd", "efgh", "ijkl"])) error stop 4

d = mk_wd(2)
if (any(d%x /= [2, 4])) error stop 5
if (d%s /= "zz") error stop 6

do i = 1, 3
    dv(i) = mk_wd(i)
end do
do i = 1, 3
    if (any(dv(i)%x /= [i, 2*i])) error stop 7
    if (len(dv(i)%s) /= i) error stop 8
end do

h%in = mk_wd(3)
if (any(h%in%x /= [3, 6])) error stop 9
if (h%in%s /= "zzz") error stop 10

p => t
p = mk1()
if (t%c /= 3) error stop 11

s = mk_class()
if (s%c /= 9) error stop 12

! `g` is finalized before it is defined and the result after the statement
g%c = 0
nfin = 0
g = mk_tf()
if (g%c /= 5) error stop 13
if (nfin /= 2) error stop 14
if (any(fin_c /= [0, 5])) error stop 15

print *, s%c, sa%c, ss%c, d%x, h%in%x, t%c, g%c

contains

function mk1() result(r)
    type(w), allocatable :: r
    allocate(r)
    r%c = 3
end function

function mk_arr() result(r)
    type(wa), allocatable :: r
    allocate(r)
    r%c = [1, 2, 3]
end function

function mk_str() result(r)
    type(ws), allocatable :: r
    allocate(r)
    r%c = ["abcd", "efgh", "ijkl"]
end function

end program
