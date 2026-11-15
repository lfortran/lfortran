module inline_loop_subroutine_calls_01_m
implicit none
integer(4), save :: counter_calls = 0
integer(4), save :: total = 0
real(4), dimension(:), allocatable, save :: use_saved_w

contains

subroutine allocatable_local(n, s)
    integer(4), intent(in) :: n
    integer(4), intent(inout) :: s
    integer(4), dimension(:), allocatable :: t
    allocate(t(n))
    t = 1
    s = s + Sum(t)
end subroutine allocatable_local

subroutine copy_bounds(d, w)
    real(4), dimension(:), intent(in) :: d
    real(4), dimension(:), allocatable, intent(inout) :: w
    w = d
end subroutine copy_bounds

subroutine counter(k)
    integer(4), intent(in) :: k
    counter_calls = counter_calls + 1
    total = total + k*counter_calls
end subroutine counter

subroutine factorial(n, r)
    integer(4), intent(in) :: n
    integer(4), intent(out) :: r
    if (n <= 1) then
        r = 1
    else
        call factorial(n - 1, r)
        r = r*n
    end if
end subroutine factorial

subroutine first_bound(d, lb)
    real(4), dimension(:), intent(in) :: d
    integer(4), intent(out) :: lb
    lb = lbound(d, 1)
end subroutine first_bound

subroutine first_element(d, v)
    real(4), dimension(:), intent(in) :: d
    real(4), intent(out) :: v
    v = d(1)
end subroutine first_element

subroutine fresh(x, n)
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(out) :: x
    allocate(x(n))
    x = real(1, kind=4)
end subroutine fresh

subroutine grow(x, n)
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(inout) :: x
    x = [x, Real(n)]
end subroutine grow

subroutine optional_step(k, m)
    integer(4), intent(inout) :: k
    integer(4), intent(in), optional :: m
    if (Present(m)) then
        k = k + m
    else
        k = k + 1
    end if
end subroutine optional_step

subroutine run_module_calls()
    integer(4), dimension(2) :: __inlined_pair
    integer(4) :: __inlined_t
    real(4), dimension(:), allocatable :: a
    real(4), dimension(:), allocatable :: b
    real(4), dimension(:), allocatable :: c
    integer(4) :: f
    integer(4) :: i
    integer(4) :: k
    integer(4) :: lb
    integer(4) :: m
    integer(4) :: p
    integer(4) :: q
    real(4) :: r
    real(4) :: s
    real(4) :: v
    real(4), dimension(:), allocatable :: w
    real(4), dimension(:), allocatable :: x
    allocate(a(5), b(5))
    a = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00, 5.00000000e+00]
    b = a
    do i = 1, 3
        counter_calls = counter_calls + 1
        total = total + i*counter_calls
    end do
    call counter(10)
    if (total /= 1 + 4 + 9 + 40) then
        error stop
    end if
    s = real(0, kind=4)
    do i = 1, 5
        if (.not. Allocated(use_saved_w)) then
            allocate(use_saved_w(size(a)))
        end if
        use_saved_w = a + b
        r = use_saved_w(1 + Mod(i, size(use_saved_w)))
        s = s + r
    end do
    if (Abs(s - real(30, kind=4)) > 9.99999997e-07) then
        error stop
    end if
    c = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00, 5.00000000e+00, 6.00000000e+00, 7.00000000e+00]
    s = real(0, kind=4)
    do i = 1, 2
        if (.not. Allocated(use_saved_w)) then
            allocate(use_saved_w(size(c)))
        end if
        use_saved_w = c + c
        r = use_saved_w(1 + Mod(i, size(use_saved_w)))
        s = s + r
    end do
    if (Abs(s - real(10, kind=4)) > 9.99999997e-07) then
        error stop
    end if
    allocate(x(0))
    do i = 1, 4
        x = [x, Real(i)]
    end do
    if (size(x) /= 4) then
        error stop
    end if
    if (Any(x /= real([1, 2, 3, 4], kind=4))) then
        error stop
    end if
    do i = 1, 3
        allocate(x(i))
        x = real(1, kind=4)
        if (size(x) /= i) then
            error stop
        end if
        if (Any(x /= real(1, kind=4))) then
            error stop
        end if
    end do
    p = 1
    q = 2
    k = 0
    do i = 1, 3
        __inlined_t = p
        p = q
        q = __inlined_t
        __inlined_pair = [p, q]
        k = k + Sum(__inlined_pair)
    end do
    if (p /= 2 .or. q /= 1 .or. k /= 9) then
        error stop
    end if
    do i = 1, 2
        call first_bound(a, lb)
        if (lb /= 1) then
            error stop
        end if
        call copy_bounds(a, w)
        if (lbound(w, 1) /= 1 .or. ubound(w, 1) /= 5) then
            error stop
        end if
        call first_element(a, v)
        if (v /= real(1, kind=4)) then
            error stop
        end if
    end do
    k = 0
    do i = 1, 2
        call optional_step(k)
        call optional_step(k, 10)
    end do
    if (k /= 22) then
        error stop
    end if
    m = 0
    do i = 1, 4
        call factorial(i, f)
        m = m + f
    end do
    if (m /= 1 + 2 + 6 + 24) then
        error stop
    end if
    k = 0
    do i = 1, 3
        call allocatable_local(i, k)
    end do
    if (k /= 6) then
        error stop
    end if
end subroutine run_module_calls

subroutine swap_sum(p, q, s)
    integer(4), intent(inout) :: p
    integer(4), dimension(2) :: pair
    integer(4), intent(inout) :: q
    integer(4), intent(inout) :: s
    integer(4) :: t
    t = p
    p = q
    q = t
    pair = [p, q]
    s = s + Sum(pair)
end subroutine swap_sum

subroutine use_saved(a, b, i, r)
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(:), intent(in) :: b
    integer(4), intent(in) :: i
    real(4), intent(out) :: r
    if (.not. Allocated(use_saved_w)) then
        allocate(use_saved_w(size(a)))
    end if
    use_saved_w = a + b
    r = use_saved_w(1 + Mod(i, size(use_saved_w)))
end subroutine use_saved

end module inline_loop_subroutine_calls_01_m

program inline_loop_subroutine_calls_01
use inline_loop_subroutine_calls_01_m, only: allocatable_local
use inline_loop_subroutine_calls_01_m, only: copy_bounds
use inline_loop_subroutine_calls_01_m, only: counter
use inline_loop_subroutine_calls_01_m, only: factorial
use inline_loop_subroutine_calls_01_m, only: first_bound
use inline_loop_subroutine_calls_01_m, only: first_element
use inline_loop_subroutine_calls_01_m, only: fresh
use inline_loop_subroutine_calls_01_m, only: grow
use inline_loop_subroutine_calls_01_m, only: optional_step
use inline_loop_subroutine_calls_01_m, only: run_module_calls
use inline_loop_subroutine_calls_01_m, only: swap_sum
use inline_loop_subroutine_calls_01_m, only: total
use inline_loop_subroutine_calls_01_m, only: use_saved
implicit none
integer(4) :: __inlined_j
integer(4) :: i
integer(4) :: last
integer(4) :: n
integer(4), save :: tick_calls = 0
integer(4) :: ticks
real(4), dimension(:), allocatable :: z
call run_module_calls()
ticks = 0
do i = 1, 3
    tick_calls = tick_calls + 1
    ticks = tick_calls
end do
call tick()
if (ticks /= 4) then
    error stop
end if
allocate(z(0))
do i = 1, 4
    z = [z, Real(i)]
end do
if (size(z) /= 4) then
    error stop
end if
if (Any(z /= real([1, 2, 3, 4], kind=4))) then
    error stop
end if
n = 1
do i = 1, 3
    __inlined_j = n*2
    n = n + __inlined_j
    last = __inlined_j
end do
if (n /= 27 .or. last /= 18) then
    error stop
end if
print *, "ok"

contains

subroutine append(j)
    integer(4), intent(in) :: j
    z = [z, Real(j)]
end subroutine append

subroutine bump(j)
    integer(4), intent(in) :: j
    n = n + j
    last = j
end subroutine bump

subroutine tick()
    tick_calls = tick_calls + 1
    ticks = tick_calls
end subroutine tick

end program inline_loop_subroutine_calls_01
