! Calls of small subroutines in loops, which --fast inlines, or not; the
! results must be those of the calls either way.
module inline_loop_subroutine_calls_01_m
implicit none
integer :: total = 0

contains

! A SAVE local, which every call shares, inlined or not
subroutine counter(k)
    integer, intent(in) :: k
    integer, save :: calls = 0
    calls = calls + 1
    total = total + k * calls
end subroutine

! A SAVE work array allocated by the first call
subroutine use_saved(a, b, i, r)
    real, intent(in) :: a(:), b(:)
    integer, intent(in) :: i
    real, intent(out) :: r
    real, allocatable, save :: w(:)
    if (.not. allocated(w)) allocate(w(size(a)))
    w = a + b
    r = w(1 + mod(i, size(w)))
end subroutine

! An allocatable dummy argument is its actual argument
subroutine grow(x, n)
    real, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: n
    x = [x, real(n)]
end subroutine

! An INTENT(OUT) allocatable dummy argument is deallocated on entry
subroutine fresh(x, n)
    real, allocatable, intent(out) :: x(:)
    integer, intent(in) :: n
    allocate(x(n))
    x = 1
end subroutine

! Local variables are undefined on entry
subroutine swap_sum(p, q, s)
    integer, intent(inout) :: p, q, s
    integer :: t
    integer :: pair(2)
    t = p
    p = q
    q = t
    pair = [p, q]
    s = s + sum(pair)
end subroutine

! The lower bound of an assumed-shape dummy argument is 1
subroutine first_bound(d, lb)
    real, intent(in) :: d(:)
    integer, intent(out) :: lb
    lb = lbound(d, 1)
end subroutine

! ... which an allocatable assigned the dummy argument takes
subroutine copy_bounds(d, w)
    real, intent(in) :: d(:)
    real, allocatable, intent(inout) :: w(:)
    w = d
end subroutine

! ... and from which its elements are numbered
subroutine first_element(d, v)
    real, intent(in) :: d(:)
    real, intent(out) :: v
    v = d(1)
end subroutine

subroutine optional_step(k, m)
    integer, intent(inout) :: k
    integer, intent(in), optional :: m
    if (present(m)) then
        k = k + m
    else
        k = k + 1
    end if
end subroutine

recursive subroutine factorial(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    if (n <= 1) then
        r = 1
    else
        call factorial(n - 1, r)
        r = r * n
    end if
end subroutine

subroutine allocatable_local(n, s)
    integer, intent(in) :: n
    integer, intent(inout) :: s
    integer, allocatable :: t(:)
    allocate(t(n))
    t = 1
    s = s + sum(t)
end subroutine

! The calls in the loops of a procedure of the same module
subroutine run_module_calls()
    real, allocatable :: a(:), b(:), c(:), x(:), w(:)
    real :: r, s, v
    integer :: i, k, lb, f, m, p, q
    allocate(a(0:4), b(0:4))
    a = [1, 2, 3, 4, 5]
    b = a

    ! SAVE locals are shared by the inlined and the remaining calls
    do i = 1, 3
        call counter(i)
    end do
    call counter(10)
    if (total /= 1 + 4 + 9 + 40) error stop 1

    s = 0
    do i = 1, 5
        call use_saved(a, b, i, r)
        s = s + r
    end do
    if (abs(s - 30) > 1e-6) error stop 2
    c = [1, 2, 3, 4, 5, 6, 7]
    s = 0
    do i = 1, 2
        call use_saved(c, c, i, r)
        s = s + r
    end do
    if (abs(s - 10) > 1e-6) error stop 3

    allocate(x(0))
    do i = 1, 4
        call grow(x, i)
    end do
    if (size(x) /= 4) error stop 4
    if (any(x /= [1, 2, 3, 4])) error stop 5

    do i = 1, 3
        call fresh(x, i)
        if (size(x) /= i) error stop 6
        if (any(x /= 1)) error stop 7
    end do

    p = 1
    q = 2
    k = 0
    do i = 1, 3
        call swap_sum(p, q, k)
    end do
    if (p /= 2 .or. q /= 1 .or. k /= 9) error stop 8

    ! What depends on the lower bounds of the actual argument, 0 here
    do i = 1, 2
        call first_bound(a, lb)
        if (lb /= 1) error stop 9
        call copy_bounds(a, w)
        if (lbound(w, 1) /= 1 .or. ubound(w, 1) /= 5) error stop 10
        call first_element(a, v)
        if (v /= 1) error stop 11
    end do

    k = 0
    do i = 1, 2
        call optional_step(k)
        call optional_step(k, 10)
    end do
    if (k /= 22) error stop 12

    m = 0
    do i = 1, 4
        call factorial(i, f)
        m = m + f
    end do
    if (m /= 1 + 2 + 6 + 24) error stop 13

    k = 0
    do i = 1, 3
        call allocatable_local(i, k)
    end do
    if (k /= 6) error stop 14
end subroutine

end module

program inline_loop_subroutine_calls_01
use inline_loop_subroutine_calls_01_m
implicit none
real, allocatable :: z(:)
integer :: i, n, last, ticks

call run_module_calls()

! The calls in the loops of the host of internal procedures
ticks = 0
do i = 1, 3
    call tick()
end do
call tick()
if (ticks /= 4) error stop 15

! Host associated variables
allocate(z(0))
do i = 1, 4
    call append(i)
end do
if (size(z) /= 4) error stop 16
if (any(z /= [1, 2, 3, 4])) error stop 17

! An actual argument that is an expression is evaluated before the call
n = 1
do i = 1, 3
    call bump(n * 2)
end do
if (n /= 27 .or. last /= 18) error stop 18

print *, "ok"

contains

subroutine tick()
    integer, save :: calls = 0
    calls = calls + 1
    ticks = calls
end subroutine

subroutine append(j)
    integer, intent(in) :: j
    z = [z, real(j)]
end subroutine

subroutine bump(j)
    integer, intent(in) :: j
    n = n + j
    last = j
end subroutine

end program
