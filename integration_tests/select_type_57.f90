program select_type_57
! `class is` guard on allocatable and pointer class(t) array selectors
implicit none
type :: t
    integer :: i = 1
end type
type, extends(t) :: u
    integer :: j = 2
end type
class(t), allocatable :: a(:)
class(t), pointer :: p(:)
class(t), allocatable :: a2(:,:)
class(*), allocatable :: s(:)
class(*), pointer :: sp(:)
integer :: n

allocate(a(3))
n = 0
select type (a)
class is (t)
    n = 1
    if (size(a) /= 3) error stop
    a(2)%i = 5
    if (a(2)%i /= 5) error stop
end select
if (n /= 1) error stop
if (a(2)%i /= 5) error stop
if (a(1)%i /= 1) error stop
deallocate(a)

allocate(u :: a(2))
n = 0
select type (a)
type is (t)
    n = 1
class is (u)
    n = 2
    if (a(2)%j /= 2) error stop
    a(2)%j = 7
class is (t)
    n = 3
end select
if (n /= 2) error stop
select type (q => a)
class is (u)
    if (q(2)%j /= 7) error stop
    if (q(1)%j /= 2) error stop
class default
    error stop
end select

allocate(p(2))
n = 0
select type (p)
class is (t)
    n = 1
    if (p(2)%i /= 1) error stop
    p(1)%i = 4
end select
if (n /= 1) error stop
if (p(1)%i /= 4) error stop
deallocate(p)

allocate(u :: p(2))
n = 0
select type (p)
class is (t)
    n = 1
    if (p(2)%i /= 1) error stop
end select
if (n /= 1) error stop
n = 0
select type (p)
class is (u)
    n = 1
    if (p(2)%i /= 1) error stop
    if (p(2)%j /= 2) error stop
    p(2)%j = 9
class default
    error stop
end select
if (n /= 1) error stop
select type (p)
type is (u)
    if (p(2)%j /= 9) error stop
class default
    error stop
end select
deallocate(p)

allocate(u :: a2(2,3))
n = 0
select type (a2)
class is (u)
    n = 1
    if (size(a2, 1) /= 2 .or. size(a2, 2) /= 3) error stop
    if (ubound(a2, 2) /= 3) error stop
    a2(2,3)%j = 5
class default
    error stop
end select
if (n /= 1) error stop
select type (a2)
type is (u)
    if (a2(2,3)%j /= 5) error stop
    if (a2(1,1)%j /= 2) error stop
class default
    error stop
end select
deallocate(a2)

allocate(u :: s(3))
n = 0
select type (s)
class is (t)
    n = 1
    if (size(s) /= 3) error stop
    s(2)%i = 5
class default
    error stop
end select
if (n /= 1) error stop
select type (s)
type is (u)
    if (s(2)%i /= 5) error stop
    if (s(3)%i /= 1) error stop
class default
    error stop
end select
deallocate(s)

allocate(u :: sp(2))
n = 0
select type (sp)
class is (u)
    n = 1
    if (size(sp) /= 2) error stop
    sp(2)%j = 6
class default
    error stop
end select
if (n /= 1) error stop
select type (sp)
type is (u)
    if (sp(2)%j /= 6) error stop
class default
    error stop
end select
deallocate(sp)
print *, "ok"
end program
