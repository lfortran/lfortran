program intrinsics_483
! Array intrinsics on allocatable, pointer and allocatable component arrays
! whose lower bounds are not 1 (#14369)
implicit none
type :: t
    integer, allocatable :: a(:)
end type
integer, allocatable :: a(:), m(:,:)
integer, allocatable, target :: b(:)
integer, pointer :: p(:), q(:)
integer, target :: e(8)
logical, allocatable :: msk(:)
character(:), allocatable :: s(:)
type(t) :: x
integer :: i(1), j(2)

allocate(a(0:3))
a = [10, 40, 30, 20]
allocate(b(-2:1))
b = [10, 40, 30, 20]
p => b
allocate(x%a(0:3))
x%a = [10, 40, 30, 20]
e = [10, 0, 40, 0, 30, 0, 20, 0]
q => e(1:7:2)
allocate(msk(0:3))
msk = [.false., .true., .true., .false.]

i = maxloc(a)
if (i(1) /= 2) error stop 1
i = minloc(a)
if (i(1) /= 1) error stop 2
i = findloc(a, 30)
if (i(1) /= 3) error stop 3
if (any(pack(a, [.false., .true., .true., .false.]) /= [40, 30])) error stop 4
if (dot_product(a, [1, 2, 3, 4]) /= 260) error stop 5
if (dot_product([1, 2, 3, 4], a) /= 260) error stop 6
if (any(cshift(a, 1) /= [40, 30, 20, 10])) error stop 7
if (any(eoshift(a, 1) /= [40, 30, 20, 0])) error stop 8

i = maxloc(p)
if (i(1) /= 2) error stop 9
i = minloc(p)
if (i(1) /= 1) error stop 10
i = findloc(p, 30)
if (i(1) /= 3) error stop 11
if (any(pack(p, [.false., .true., .true., .false.]) /= [40, 30])) error stop 12
if (dot_product(p, [1, 2, 3, 4]) /= 260) error stop 13
if (any(cshift(p, 1) /= [40, 30, 20, 10])) error stop 14
if (any(eoshift(p, 1) /= [40, 30, 20, 0])) error stop 15

i = maxloc(x%a)
if (i(1) /= 2) error stop 16
i = minloc(x%a)
if (i(1) /= 1) error stop 17
i = findloc(x%a, 30)
if (i(1) /= 3) error stop 18
if (any(pack(x%a, [.false., .true., .true., .false.]) /= [40, 30])) error stop 19
if (dot_product(x%a, [1, 2, 3, 4]) /= 260) error stop 20
if (any(cshift(x%a, 1) /= [40, 30, 20, 10])) error stop 21
if (any(eoshift(x%a, 1) /= [40, 30, 20, 0])) error stop 22

! Non-contiguous (strided) pointer
i = maxloc(q)
if (i(1) /= 2) error stop 23
i = findloc(q, 30)
if (i(1) /= 3) error stop 24
if (any(pack(q, [.false., .true., .true., .false.]) /= [40, 30])) error stop 25
if (dot_product(q, [1, 2, 3, 4]) /= 260) error stop 26
if (any(cshift(q, 1) /= [40, 30, 20, 10])) error stop 27
if (any(eoshift(q, 1) /= [40, 30, 20, 0])) error stop 28
if (lbound(cshift(q, 1), 1) /= 1) error stop 29

! Results are indexed from 1 when the argument extent is not known at
! compile time
if (lbound(cshift(a, 1), 1) /= 1) error stop 30
if (ubound(cshift(a, 1), 1) /= 4) error stop 31
if (lbound(eoshift(a, 1), 1) /= 1) error stop 32
if (lbound(pack(a, msk), 1) /= 1) error stop 33

! Both arguments with lower bounds other than 1
i = maxloc(p, msk)
if (i(1) /= 2) error stop 34
i = minloc(p, msk)
if (i(1) /= 3) error stop 35
i = findloc(p, 30, mask=msk)
if (i(1) /= 3) error stop 36
if (any(pack(p, msk) /= [40, 30])) error stop 37
if (dot_product(p, a) /= 3000) error stop 38

allocate(m(0:1, 0:1))
m(0, 0) = 1
m(1, 0) = 2
m(0, 1) = 3
m(1, 1) = 4
j = maxloc(m)
if (any(j /= [2, 2])) error stop 39
if (any(matmul(m, [1, 1]) /= [4, 6])) error stop 40
if (any(transpose(m) /= reshape([1, 3, 2, 4], [2, 2]))) error stop 41
if (any(lbound(transpose(m)) /= [1, 1])) error stop 42
if (lbound(matmul(m, [1, 1]), 1) /= 1) error stop 43

allocate(character(2) :: s(0:2))
s = ["bb", "cc", "aa"]
i = maxloc(s)
if (i(1) /= 2) error stop 44
i = findloc(s, "aa")
if (i(1) /= 3) error stop 45
if (any(cshift(s, 1) /= ["cc", "aa", "bb"])) error stop 46
print *, "ok"
end program
