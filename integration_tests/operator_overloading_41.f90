module operator_overloading_41_mod
implicit none
type :: t
    integer :: v = 0
end type
interface operator(.and.)
    module procedure and_tl
end interface
interface operator(.or.)
    module procedure or_tl
end interface
interface operator(.eqv.)
    module procedure eqv_tt
end interface
contains
logical function and_tl(a, b)
    type(t), intent(in) :: a
    logical, intent(in) :: b
    and_tl = a%v > 0 .and. b
end function
logical function or_tl(a, b)
    type(t), intent(in) :: a
    logical, intent(in) :: b
    or_tl = a%v > 0 .or. b
end function
logical function eqv_tt(a, b)
    type(t), intent(in) :: a, b
    eqv_tt = a%v == b%v
end function
subroutine run_and(r)
    logical, intent(out) :: r
    type(t) :: x
    x%v = 1
    r = x .and. .true.
end subroutine
subroutine run_or(r)
    logical, intent(out) :: r
    type(t) :: x
    x%v = 0
    r = x .or. .false.
end subroutine
logical function run_eqv(i, j) result(r)
    integer, intent(in) :: i, j
    type(t) :: x, y
    x%v = i
    y%v = j
    r = x .eqv. y
end function
end module

program operator_overloading_41
use operator_overloading_41_mod
implicit none
logical :: r
call run_and(r)
print *, r
if (.not. r) error stop
call run_or(r)
print *, r
if (r) error stop
if (.not. run_eqv(3, 3)) error stop
if (run_eqv(3, 4)) error stop
end program
