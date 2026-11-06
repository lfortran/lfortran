program allocate_source_component_01
! allocate(..., source=) with an allocatable or pointer derived-type
! component as the source
use iso_c_binding, only: c_int, c_double
implicit none
type, bind(c) :: ct_t
    integer(c_int) :: a
    real(c_double) :: b
end type ct_t
type :: ch_t
    type(ct_t), pointer :: p => null()
    type(ct_t), allocatable :: q
    complex, allocatable :: z
    logical, pointer :: lp => null()
end type ch_t
type :: more_t
    integer :: n = 0
    integer, allocatable :: v(:)
end type more_t
type :: e_t
    type(more_t), allocatable :: more
end type e_t
type :: f_t
    type(more_t), pointer :: more => null()
    integer, allocatable :: k
    integer, pointer :: kp => null()
    character(:), allocatable :: s
    character(:), pointer :: sp => null()
    real, pointer :: rp => null()
end type f_t
type :: box_t
    type(f_t), pointer :: e => null()
end type box_t
type :: list_t
    type(box_t), allocatable :: box(:)
end type list_t
type :: proj_t
    type(list_t) :: list
end type proj_t
type(e_t) :: a, b
type(f_t) :: c
type(more_t), allocatable :: q, r
type(proj_t), pointer :: p
class(more_t), allocatable :: cq
class(*), allocatable :: u1, u2, u3, u4
character(:), allocatable :: s1, s2
type(e_t), allocatable :: arr(:)
type(more_t), allocatable :: m1, m2
class(*), allocatable :: w
real, allocatable :: x1
type(ch_t) :: h
type(ct_t), allocatable :: y1, y2
class(*), allocatable :: v1, v2
integer :: item, copy

allocate(a%more)
a%more%n = 5
allocate(a%more%v(3))
a%more%v = [1, 2, 3]
allocate(q, source=a%more)
if (q%n /= 5) error stop
if (any(q%v /= [1, 2, 3])) error stop
q%v(1) = 10
if (a%more%v(1) /= 1) error stop

allocate(c%more)
c%more%n = 7
allocate(c%more%v(2))
c%more%v = [4, 5]
allocate(r, source=c%more)
if (r%n /= 7) error stop
if (any(r%v /= [4, 5])) error stop

allocate(b%more, source=a%more)
if (b%more%n /= 5) error stop
if (any(b%more%v /= [1, 2, 3])) error stop
b%more%n = 6
if (a%more%n /= 5) error stop

allocate(p)
allocate(p%list%box(2))
allocate(p%list%box(1)%e, p%list%box(2)%e)
item = 1
copy = 2
allocate(p%list%box(item)%e%more)
p%list%box(item)%e%more%n = 8
allocate(p%list%box(item)%e%more%v(1))
p%list%box(item)%e%more%v = [9]
allocate(p%list%box(copy)%e%more, source=p%list%box(item)%e%more)
if (p%list%box(copy)%e%more%n /= 8) error stop
if (any(p%list%box(copy)%e%more%v /= [9])) error stop
p%list%box(copy)%e%more%n = 1
if (p%list%box(item)%e%more%n /= 8) error stop
allocate(cq, source=a%more)
if (cq%n /= 5) error stop
if (any(cq%v /= [1, 2, 3])) error stop

allocate(c%k, c%kp)
c%k = 11
c%kp = 12
allocate(u1, source=c%k)
allocate(u2, source=c%kp)
select type (u1)
type is (integer)
    if (u1 /= 11) error stop
class default
    error stop
end select
select type (u2)
type is (integer)
    if (u2 /= 12) error stop
class default
    error stop
end select

c%s = "hello"
allocate(character(5) :: c%sp)
c%sp = "world"
allocate(u3, source=c%s)
allocate(u4, source=c%sp)
select type (u3)
type is (character(*))
    if (len(u3) /= 5) error stop
    if (u3 /= "hello") error stop
class default
    error stop
end select
select type (u4)
type is (character(*))
    if (len(u4) /= 5) error stop
    if (u4 /= "world") error stop
class default
    error stop
end select
allocate(s1, source=c%s)
allocate(s2, source=c%sp)
if (len(s1) /= 5 .or. s1 /= "hello") error stop
if (len(s2) /= 5 .or. s2 /= "world") error stop

allocate(c%rp)
c%rp = 2.5
allocate(x1, source=c%rp)
if (abs(x1 - 2.5) > 1e-6) error stop

allocate(m1, mold=a%more)
if (m1%n /= 0) error stop
if (allocated(m1%v)) error stop
allocate(w, mold=c%more)
select type (w)
type is (more_t)
    if (w%n /= 0) error stop
class default
    error stop
end select

allocate(arr(2))
allocate(arr(2)%more)
arr(2)%more%n = 13
allocate(arr(2)%more%v(2))
arr(2)%more%v = [6, 7]
allocate(m2, source=arr(2)%more)
if (m2%n /= 13) error stop
if (any(m2%v /= [6, 7])) error stop

allocate(h%p, h%q)
h%p%a = 3
h%p%b = 1.5d0
h%q%a = 4
h%q%b = 2.5d0
allocate(y1, source=h%p)
allocate(y2, source=h%q)
if (y1%a /= 3 .or. y1%b /= 1.5d0) error stop
if (y2%a /= 4 .or. y2%b /= 2.5d0) error stop

allocate(h%z, h%lp)
h%z = (1.0, 2.0)
h%lp = .true.
allocate(v1, source=h%z)
allocate(v2, source=h%lp)
select type (v1)
type is (complex)
    if (v1 /= (1.0, 2.0)) error stop
class default
    error stop
end select
select type (v2)
type is (logical)
    if (.not. v2) error stop
class default
    error stop
end select

call check_class_dummy(a)
print *, q%n, r%n, cq%n, b%more%n, p%list%box(copy)%e%more%n
deallocate(c%more, c%kp, c%sp, c%rp, h%p, h%lp)
deallocate(p%list%box(1)%e%more, p%list%box(2)%e%more)
deallocate(p%list%box(1)%e, p%list%box(2)%e)
deallocate(p)

contains

subroutine check_class_dummy(x)
    class(e_t), intent(in) :: x
    type(more_t), allocatable :: t
    allocate(t, source=x%more)
    if (t%n /= 5) error stop
    if (any(t%v /= [1, 2, 3])) error stop
end subroutine check_class_dummy

end program allocate_source_component_01
