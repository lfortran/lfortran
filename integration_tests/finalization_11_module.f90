module finalization_11_module
implicit none
integer :: ncalls = 0, nfin = 0
type :: t
    integer :: v = 0
contains
    final :: fin
    procedure :: mul_ti
    generic :: operator(*) => mul_ti
end type
interface operator(+)
    module procedure add_ti
end interface
interface operator(==)
    module procedure eq_ti
end interface
interface operator(-)
    module procedure neg_t, sub_tt
end interface
interface operator(//)
    module procedure cat_tt
end interface
interface operator(.plus.)
    module procedure add_ti
end interface
contains
subroutine fin(x)
    type(t), intent(inout) :: x
    nfin = nfin + 1
end subroutine
function mk(i) result(r)
    integer, intent(in) :: i
    type(t) :: r
    ncalls = ncalls + 1
    r%v = i
end function
integer function add_ti(a, b)
    type(t), intent(in) :: a
    integer, intent(in) :: b
    add_ti = a%v + b
end function
logical function eq_ti(a, b)
    type(t), intent(in) :: a
    integer, intent(in) :: b
    eq_ti = a%v == b
end function
function wrap(a) result(r)
    type(t), intent(in) :: a
    type(t) :: r
    ncalls = ncalls + 1
    r%v = a%v
end function
integer function mul_ti(a, b)
    class(t), intent(in) :: a
    integer, intent(in) :: b
    mul_ti = a%v * b
end function
function sub_tt(a, b) result(r)
    type(t), intent(in) :: a, b
    type(t) :: r
    ncalls = ncalls + 1
    r%v = a%v - b%v
end function
integer function neg_t(a)
    type(t), intent(in) :: a
    neg_t = -a%v
end function
integer function cat_tt(a, b)
    type(t), intent(in) :: a, b
    cat_tt = 10*a%v + b%v
end function
! Checks the value `k` of a statement, and that it called `mk` `calls`
! times and finalized `fins` results (not checked if `fins` < 0), then
! resets the counters for the next statement.
subroutine check(k, expected, calls, fins)
    integer, intent(in) :: k, expected, calls, fins
    print *, k, ncalls, nfin
    if (k /= expected) error stop 1
    if (ncalls /= calls) error stop 2
    if (fins >= 0 .and. nfin /= fins) error stop 3
    ncalls = 0
    nfin = 0
end subroutine
subroutine run()
    integer :: k, i
    k = mk(1) + 2
    call check(k, 3, 1, 1)
    k = -mk(4)
    call check(k, -4, 1, 1)
    k = mk(1) // mk(2)
    call check(k, 12, 2, 2)
    k = mk(5) .plus. 1
    call check(k, 6, 1, 1)
    k = wrap(mk(2)) * 3
    call check(k, 6, 2, 2)
    k = (mk(5) - wrap(mk(2))) * 2
    call check(k, 6, 4, 4)
    k = 0
    if (mk(3) == 3) k = 1
    call check(k, 1, 1, -1)
    k = 0
    do i = 1, mk(2) + 0
        k = k + 1
    end do
    call check(k, 2, 1, -1)
end subroutine
end module
