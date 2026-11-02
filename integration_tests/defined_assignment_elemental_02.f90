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
contains
elemental subroutine copy(to, from)
    type(t), intent(inout) :: to
    type(t), intent(in) :: from
    to%k = from%k + 10
end subroutine copy
end module defined_assignment_elemental_02_m1

module defined_assignment_elemental_02_m2
implicit none
type :: s
    integer :: k = 0
contains
    procedure :: copy
    generic :: assignment(=) => copy
end type s
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
type(t) :: a(3), b(3), c(3, 3)
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
c%k = reshape([(i, i = 1, 9)], [3, 3])
c(2, :) = a
if (any(c(2, :)%k /= [11, 12, 13])) error stop 6
if (any(c(1, :)%k /= [1, 4, 7])) error stop 7
end subroutine check_interface

subroutine check_type_bound(n)
use defined_assignment_elemental_02_m2
implicit none
integer, intent(in) :: n
type(s) :: a(3), b(3), c(3, 3)
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
c%k = reshape([(i, i = 1, 9)], [3, 3])
c(2, :) = a
if (any(c(2, :)%k /= [11, 12, 13])) error stop 16
if (any(c(1, :)%k /= [1, 4, 7])) error stop 17
end subroutine check_type_bound

program defined_assignment_elemental_02
implicit none
call check_interface(3)
call check_type_bound(3)
print *, "ok"
end program defined_assignment_elemental_02
