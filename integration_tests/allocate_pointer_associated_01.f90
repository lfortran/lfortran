! ALLOCATE of a pointer that is already associated creates a new target
! and breaks the old association; it is not an error (F2023 9.7.1.4).
! Each old target is kept through a second pointer, checked to be unchanged
! and deallocated at the end, so the test is also leak-free.
module allocate_pointer_associated_01_mod
implicit none
real, pointer :: fnl(:,:) => null()
type :: base
    integer :: k = 3
end type
type, extends(base) :: ext
    integer :: m = 11
end type
type :: holder
    integer, pointer :: v(:) => null()
    real, pointer :: r => null()
end type
contains
subroutine alloc_fnl(n, ierr)
    integer, intent(in) :: n
    integer, intent(out) :: ierr
    allocate(fnl(n, 2), stat=ierr)
end subroutine
subroutine alloc_dummy(x, n)
    integer, pointer, intent(inout) :: x(:)
    integer, intent(in) :: n
    allocate(x(n))
end subroutine
end module

program allocate_pointer_associated_01
use allocate_pointer_associated_01_mod
implicit none
type :: t
    integer :: a = 7
end type
real, pointer :: p(:), p1(:), p2(:), q(:,:), q1(:,:), fnl1(:,:), r1
integer, target :: blk(10)
integer, allocatable, target :: ablk(:)
integer, pointer :: ip(:), d(:), d1(:), v1(:)
integer, pointer :: s, s1, s2
character(:), pointer :: c, c1, c2
character(len=4), pointer :: cf, cf1
type(t), pointer :: dt, dt1, dta(:), dta1(:), dta2(:)
class(base), pointer :: cp, cp1, cpa(:), cpa1(:), cpa2(:)
class(*), pointer :: u, u1, ua(:), ua1(:)
type(holder) :: h
character(len=6), target :: buf6
type(ext), target :: et
integer :: ierr

! Array pointer, with and without STAT=
allocate(p(3), stat=ierr)
if (ierr /= 0 .or. size(p) /= 3) error stop 1
p = 1
p1 => p
allocate(p(5), stat=ierr)
if (ierr /= 0 .or. size(p) /= 5) error stop 2
p = 2
p2 => p
allocate(p(4))
if (size(p) /= 4) error stop 3
p = 3
if (size(p1) /= 3 .or. any(p1 /= 1)) error stop 4
if (size(p2) /= 5 .or. any(p2 /= 2)) error stop 5
deallocate(p, p1, p2)

allocate(q(2,2))
q = 1
q1 => q
allocate(q(3,4), stat=ierr)
q = 2
if (ierr /= 0 .or. size(q) /= 12) error stop 6
if (size(q1) /= 4 .or. any(q1 /= 1)) error stop 7
deallocate(q, q1)

! The previous target is not changed (#13028)
blk = 4
ip => blk
allocate(ip(3))
ip = 1
if (size(ip) /= 3 .or. any(blk /= 4)) error stop 8
deallocate(ip)

allocate(ablk(6))
ablk = 9
ip => ablk
allocate(ip(2), stat=ierr)
ip = 1
if (ierr /= 0 .or. size(ip) /= 2) error stop 9
if (size(ablk) /= 6 .or. any(ablk /= 9)) error stop 10
deallocate(ip, ablk)

! Scalar pointers
allocate(s)
s = 1
s1 => s
allocate(s)
s = 2
s2 => s
allocate(s, stat=ierr)
s = 3
if (ierr /= 0 .or. s1 /= 1 .or. s2 /= 2) error stop 11
deallocate(s, s1, s2)

allocate(character(5) :: c)
c = 'abcde'
c1 => c
allocate(character(3) :: c, stat=ierr)
if (ierr /= 0 .or. len(c) /= 3) error stop 12
c = 'fgh'
c2 => c
allocate(character(4) :: c)
if (len(c) /= 4) error stop 13
c = 'ijkl'
if (len(c1) /= 5 .or. c1 /= 'abcde') error stop 14
if (len(c2) /= 3 .or. c2 /= 'fgh') error stop 15
deallocate(c, c1, c2)

allocate(cf)
cf = 'mnop'
cf1 => cf
allocate(cf, stat=ierr)
if (ierr /= 0) error stop 16
cf = 'qrst'
if (cf1 /= 'mnop' .or. cf /= 'qrst') error stop 17
deallocate(cf, cf1)

! Derived-type and polymorphic pointers
allocate(dt)
dt%a = 1
dt1 => dt
allocate(dt, stat=ierr)
if (ierr /= 0 .or. dt%a /= 7 .or. dt1%a /= 1) error stop 20
deallocate(dt, dt1)

allocate(dta(2))
dta%a = 1
dta1 => dta
allocate(dta(4), stat=ierr)
if (ierr /= 0 .or. size(dta) /= 4 .or. any(dta%a /= 7)) error stop 21
dta%a = 2
dta2 => dta
allocate(dta(3))
if (size(dta) /= 3) error stop 22
if (size(dta1) /= 2 .or. any(dta1%a /= 1)) error stop 23
if (size(dta2) /= 4 .or. any(dta2%a /= 2)) error stop 24
deallocate(dta, dta1, dta2)

allocate(cp)
cp%k = 1
cp1 => cp
allocate(cp, stat=ierr)
if (ierr /= 0 .or. cp%k /= 3 .or. cp1%k /= 1) error stop 25
deallocate(cp, cp1)

allocate(cpa(2))
cpa%k = 1
cpa1 => cpa
allocate(cpa(5), stat=ierr)
if (ierr /= 0 .or. size(cpa) /= 5) error stop 26
cpa%k = 2
cpa2 => cpa
allocate(cpa(6))
if (size(cpa) /= 6) error stop 27
if (size(cpa1) /= 2 .or. any(cpa1%k /= 1)) error stop 28
if (size(cpa2) /= 5 .or. any(cpa2%k /= 2)) error stop 29
deallocate(cpa, cpa1, cpa2)

allocate(integer :: u)
select type (u)
type is (integer)
    u = 1
end select
u1 => u
allocate(real :: u, stat=ierr)
if (ierr /= 0) error stop 60
select type (u)
type is (real)
    u = 2.5
class default
    error stop 61
end select
select type (u1)
type is (integer)
    if (u1 /= 1) error stop 62
class default
    error stop 63
end select
deallocate(u, u1)

! SOURCE= and MOLD=
allocate(p, source=[1.0, 2.0])
p1 => p
allocate(p, source=[3.0, 4.0, 5.0])
if (size(p) /= 3 .or. any(p /= [3.0, 4.0, 5.0])) error stop 64
if (size(p1) /= 2 .or. any(p1 /= [1.0, 2.0])) error stop 65
p2 => p
allocate(p, mold=p1)
p = 6
if (size(p) /= 2 .or. any(p1 /= [1.0, 2.0])) error stop 66
if (size(p2) /= 3 .or. any(p2 /= [3.0, 4.0, 5.0])) error stop 71
deallocate(p, p1, p2)

allocate(ua, source=[1, 2, 3])
ua1 => ua
allocate(ua, source=[4.0, 5.0])
select type (ua)
type is (real)
    if (size(ua) /= 2 .or. any(ua /= [4.0, 5.0])) error stop 67
class default
    error stop 68
end select
select type (ua1)
type is (integer)
    if (size(ua1) /= 3 .or. any(ua1 /= [1, 2, 3])) error stop 69
class default
    error stop 70
end select
deallocate(ua, ua1)

! Module, component and dummy-argument pointers
call alloc_fnl(3, ierr)
if (ierr /= 0) error stop 30
fnl = 1
fnl1 => fnl
call alloc_fnl(5, ierr)
if (ierr /= 0 .or. size(fnl, 1) /= 5) error stop 31
if (size(fnl1, 1) /= 3 .or. any(fnl1 /= 1)) error stop 32
deallocate(fnl, fnl1)

allocate(h%v(2))
h%v = 1
v1 => h%v
allocate(h%v(7), stat=ierr)
if (ierr /= 0 .or. size(h%v) /= 7) error stop 33
if (size(v1) /= 2 .or. any(v1 /= 1)) error stop 34
deallocate(h%v, v1)
allocate(h%r)
h%r = 1
r1 => h%r
allocate(h%r, stat=ierr)
if (ierr /= 0 .or. r1 /= 1) error stop 35
deallocate(h%r, r1)

allocate(d(1))
d = 1
d1 => d
call alloc_dummy(d, 4)
if (size(d) /= 4) error stop 36
if (size(d1) /= 1 .or. d1(1) /= 1) error stop 37
deallocate(d, d1)

! The previous character and polymorphic targets are not changed
buf6 = 'abcdef'
c => buf6
allocate(character(3) :: c)
c = 'xyz'
if (buf6 /= 'abcdef' .or. len(c) /= 3 .or. c /= 'xyz') error stop 38
deallocate(c)

et%k = 5
et%m = 6
cp => et
allocate(cp)
cp%k = 99
if (et%k /= 5 .or. et%m /= 6) error stop 44
select type (cp)
type is (base)
class default
    error stop 45
end select
deallocate(cp)
print *, "ok"
end program
