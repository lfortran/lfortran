! Procedures whose dummy procedure is not only called, or that have saved
! variables, are not copied into the host with --fast: the internal
! procedure is passed as a procedure value, and the results are the same.
module procedure_argument_specialization_05_m
implicit none
abstract interface
    integer function fn(x)
        integer, intent(in) :: x
    end function
end interface
procedure(fn), pointer :: saved => null()
contains
! stores the dummy in a procedure pointer
integer function store(f, x) result(r)
    procedure(fn) :: f
    integer, intent(in) :: x
    procedure(fn), pointer :: p
    p => f
    r = p(x)
    if (.not. associated(p, f)) r = -1
end function

! counts its calls in a saved variable, which a copy would not share
integer function counted(f, x) result(r)
    procedure(fn) :: f
    integer, intent(in) :: x
    integer, save :: ncalls = 0
    ncalls = ncalls + 1
    r = f(x) + 100*ncalls
end function

! a second host, which passes its internal procedure to counted
subroutine sub(m)
    integer, intent(in) :: m
    if (counted(add_m, 1) /= 411) error stop 5
contains
    integer function add_m(x)
        integer, intent(in) :: x
        add_m = x + m
    end function
end subroutine

integer function twice(x)
    integer, intent(in) :: x
    twice = 2*x
end function
end module

program procedure_argument_specialization_05
use procedure_argument_specialization_05_m
implicit none
integer :: k
k = 5
if (store(add_k, 1) /= 6) error stop 1
if (counted(add_k, 1) /= 106) error stop 2
if (counted(twice, 1) /= 202) error stop 3
if (counted(add_k, 2) /= 307) error stop 4
call sub(10)
print *, k
contains
integer function add_k(x)
    integer, intent(in) :: x
    add_k = x + k
end function
end program
