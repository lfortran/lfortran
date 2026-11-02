program allocate_source_component_01
! allocate(..., source=) with an allocatable or pointer derived-type
! component as the source
implicit none
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
print *, q%n, r%n, cq%n, b%more%n, p%list%box(copy)%e%more%n
end program allocate_source_component_01
