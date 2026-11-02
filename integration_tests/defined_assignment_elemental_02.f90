! An elemental defined assignment to an array section defines the
! elements of the section in place.
module defined_assignment_elemental_02_m1
implicit none
type :: t
    integer :: k = 0
end type t
interface assignment(=)
    module procedure copy
end interface
type :: ht
    type(t) :: arr(4)
end type ht
contains
elemental subroutine copy(to, from)
    type(t), intent(inout) :: to
    type(t), intent(in) :: from
    to%k = from%k + 10
end subroutine copy
end module defined_assignment_elemental_02_m1

module defined_assignment_elemental_02_m3
implicit none
contains
function f(x) result(r)
    integer, intent(inout) :: x(:)
    integer :: r(2)
    r(1) = x(1)
    r(2) = 5
    x(2) = 100
end function f
end module defined_assignment_elemental_02_m3

module defined_assignment_elemental_02_m2
implicit none
type :: s
    integer :: k = 0
contains
    procedure :: copy
    generic :: assignment(=) => copy
end type s
type :: hs
    type(s) :: arr(4)
end type hs
contains
elemental subroutine copy(to, from)
    class(s), intent(inout) :: to
    type(s), intent(in) :: from
    to%k = from%k + 10
end subroutine copy
end module defined_assignment_elemental_02_m2

subroutine check_interface(n)
use defined_assignment_elemental_02_m1
implicit none
integer, intent(in) :: n
type(t) :: a(3), b(3), c(3, 3), d(4)
type(ht) :: x
integer :: i
a%k = [1, 2, 3]
b(1:3) = a
if (any(b%k /= [11, 12, 13])) error stop 1
b%k = 0
b(1:2) = a(2:3)
if (any(b%k /= [12, 13, 0])) error stop 2
b%k = 0
b(1:n) = a(1:n)
if (any(b%k /= [11, 12, 13])) error stop 3
b%k = 0
b(1:3:2) = a(1:2)
if (any(b%k /= [11, 0, 12])) error stop 4
b%k = [1, 2, 3]
b(1:2) = b(2:3)
if (any(b%k /= [12, 13, 3])) error stop 5
b%k = [1, 2, 3]
b(2:3) = b(1:2)
if (any(b%k /= [1, 11, 12])) error stop 8
d%k = [1, 2, 3, 4]
d(4:1:-1) = d
if (any(d%k /= [14, 13, 12, 11])) error stop 9
x%arr%k = [1, 2, 3, 4]
x%arr(4:1:-1) = x%arr
if (any(x%arr%k /= [14, 13, 12, 11])) error stop 31
d%k = [1, 2, 3, 4]
d(2:3) = d(2)
if (any(d%k /= [1, 12, 12, 4])) error stop 32
c%k = reshape([(i, i = 1, 9)], [3, 3])
c(2, :) = a
if (any(c(2, :)%k /= [11, 12, 13])) error stop 6
if (any(c(1, :)%k /= [1, 4, 7])) error stop 7
end subroutine check_interface

subroutine check_type_bound(n)
use defined_assignment_elemental_02_m2
implicit none
integer, intent(in) :: n
type(s) :: a(3), b(3), c(3, 3), d(4)
type(hs) :: x
integer :: i
a%k = [1, 2, 3]
b(1:3) = a
if (any(b%k /= [11, 12, 13])) error stop 11
b%k = 0
b(1:2) = a(2:3)
if (any(b%k /= [12, 13, 0])) error stop 12
b%k = 0
b(1:n) = a(1:n)
if (any(b%k /= [11, 12, 13])) error stop 13
b%k = 0
b(1:3:2) = a(1:2)
if (any(b%k /= [11, 0, 12])) error stop 14
b%k = [1, 2, 3]
b(1:2) = b(2:3)
if (any(b%k /= [12, 13, 3])) error stop 15
b%k = [1, 2, 3]
b(2:3) = b(1:2)
if (any(b%k /= [1, 11, 12])) error stop 18
d%k = [1, 2, 3, 4]
d(4:1:-1) = d
if (any(d%k /= [14, 13, 12, 11])) error stop 19
x%arr%k = [1, 2, 3, 4]
x%arr(4:1:-1) = x%arr
if (any(x%arr%k /= [14, 13, 12, 11])) error stop 41
d%k = [1, 2, 3, 4]
d(2:3) = d(2)
if (any(d%k /= [1, 12, 12, 4])) error stop 42
c%k = reshape([(i, i = 1, 9)], [3, 3])
c(2, :) = a
if (any(c(2, :)%k /= [11, 12, 13])) error stop 16
if (any(c(1, :)%k /= [1, 4, 7])) error stop 17
end subroutine check_type_bound

! A function in the value defines its `intent(inout)` section argument in
! place.
subroutine check_function()
use defined_assignment_elemental_02_m3
implicit none
integer :: b(4)
b = [1, 2, 3, 4]
b(1:2) = f(b(2:3))
if (any(b /= [2, 5, 100, 4])) error stop 21
b = [1, 2, 3, 4]
b(1:2) = f(b(2:4:2))
if (any(b /= [2, 5, 3, 100])) error stop 22
end subroutine check_function

program defined_assignment_elemental_02
implicit none
call check_interface(3)
call check_type_bound(3)
call check_function()
print *, "ok"
end program defined_assignment_elemental_02
