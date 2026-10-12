program select_type_58
! `class is` guard on an assumed-shape class(t) dummy array selector
implicit none
type :: t
    integer :: i = 1
end type
type, extends(t) :: u
    integer :: j = 2
end type
type(t) :: a(2)
class(t), allocatable :: b(:)
type(u) :: c(2,3)
type(u) :: d(3)
integer :: n

n = 0
call set_t(a, n)
if (n /= 1) error stop
if (a(1)%i /= 1) error stop
if (a(2)%i /= 7) error stop

allocate(u :: b(3))
n = 0
call set_u(b, n)
if (n /= 2) error stop
select type (b)
type is (u)
    if (b(1)%j /= 2) error stop
    if (b(3)%j /= 9) error stop
    if (b(3)%i /= 4) error stop
class default
    error stop
end select

n = 0
call set_t(b, n)
if (n /= 1) error stop
if (b(2)%i /= 7) error stop

n = 0
call shifted(b, n)
if (n /= 1) error stop
if (b(1)%i /= 8) error stop

n = 0
call set_rank2(c, n)
if (n /= 2) error stop
if (c(2,3)%j /= 5) error stop
if (c(1,2)%i /= 6) error stop
if (c(1,1)%j /= 2) error stop

n = 0
call set_star(d, n)
if (n /= 2) error stop
if (d(3)%j /= 8) error stop
if (d(1)%i /= 3) error stop
if (d(2)%j /= 2) error stop
print *, "ok"

contains

subroutine set_rank2(p, n)
class(t) :: p(:,:)
integer, intent(inout) :: n
select type (p)
class is (u)
    n = 2
    if (size(p, 1) /= 2 .or. size(p, 2) /= 3) error stop
    if (ubound(p, 2) /= 3) error stop
    p(2,3)%j = 5
    p(1,2)%i = 6
class is (t)
    n = 1
end select
end subroutine

subroutine set_star(p, n)
class(*) :: p(:)
integer, intent(inout) :: n
select type (p)
class is (u)
    n = 2
    if (size(p) /= 3) error stop
    p(3)%j = 8
    p(1)%i = 3
class is (t)
    n = 1
class default
    error stop
end select
end subroutine

subroutine set_t(p, n)
class(t) :: p(:)
integer, intent(inout) :: n
select type (p)
class is (t)
    n = 1
    if (size(p) /= size(p, 1)) error stop
    if (lbound(p, 1) /= 1) error stop
    if (ubound(p, 1) /= size(p)) error stop
    if (p(1)%i /= 1) error stop
    p(2)%i = 7
    if (p(2)%i /= 7) error stop
end select
end subroutine

subroutine set_u(p, n)
class(t) :: p(:)
integer, intent(inout) :: n
select type (p)
class is (u)
    n = 2
    if (size(p) /= 3) error stop
    if (lbound(p, 1) /= 1) error stop
    if (ubound(p, 1) /= 3) error stop
    if (p(3)%j /= 2) error stop
    p(3)%j = 9
    p(3)%i = 4
    if (p(3)%j /= 9) error stop
class is (t)
    n = 1
end select
end subroutine

subroutine shifted(p, n)
class(t) :: p(0:)
integer, intent(inout) :: n
select type (q => p)
class is (t)
    n = 1
    if (size(q) /= 3) error stop
    if (lbound(q, 1) /= 0) error stop
    if (ubound(q, 1) /= 2) error stop
    q(0)%i = 8
end select
end subroutine

end program
