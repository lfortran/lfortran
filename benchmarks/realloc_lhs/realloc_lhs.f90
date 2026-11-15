! Kernels that measure the cost of reallocation on assignment (F2003
! "realloc-lhs") for allocatable arrays. See README.md in this directory.
!
! Usage: realloc_lhs <kernel> <n> <reps>
!
! Prints one line: "<kernel> <n> <reps> <seconds> <checksum>". <seconds> is
! the wall time of the <reps> iterations only (setup excluded).
!
! Each kernel perturbs its input every iteration (so the assignment cannot be
! hoisted out of the loop) and accumulates an element of the result into a
! checksum (so it cannot be removed). The element index cycles through the
! array, so every element is observable. n and reps come from the command
! line, so nothing is known at compile time.
!
! Unless a kernel is listed as requiring reallocation (append, local, resize),
! the left-hand side is already allocated with the shape of the right-hand
! side, so the program is also valid without reallocation on assignment
! (gfortran -fno-realloc-lhs, lfortran --disable-realloc-lhs-arrays) and
! measures only the overhead of the shape checks.
module realloc_kernels
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), parameter :: eps = 1e-9_dp

type :: state_t
    real(dp), allocatable :: x(:), v(:)
end type

contains

! c = a + b
subroutine k_add(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = a + b
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! x = x + dt*v (the left-hand side also appears on the right)
subroutine k_self(x, v, reps, s)
real(dp), allocatable, intent(inout) :: x(:), v(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    x = x + eps*v
    s = s + x(1 + mod(i, size(x)))
end do
end subroutine

! c = a
subroutine k_copy(a, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = a
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! c = [array constructor]
subroutine k_ctor(a, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = [a(1) + 1, a(2)*2, a(3)]
    s = s + c(1 + mod(i, 3))
end do
end subroutine

function twice(a) result(r)
real(dp), intent(in) :: a(:)
real(dp) :: r(size(a))
r = 2*a
end function

! c = f(a), explicit-shape (non-allocatable) function result
subroutine k_func(a, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = twice(a)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

function make(t, n) result(r)
real(dp), intent(in) :: t
integer, intent(in) :: n
real(dp), allocatable :: r(:)
integer :: j
allocate(r(n))
do j = 1, n
    r(j) = t + j
end do
end function

! c = f(n), allocatable function result
subroutine k_allocfn(a, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = make(a(1), size(a))
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! t = a(:,j)*2, array section of a rank-2 array, 16 columns per iteration
subroutine k_section(a, t, reps, s)
real(dp), allocatable, intent(inout) :: a(:,:), t(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, j
s = 0
do i = 1, reps
    do j = 1, size(a, 2)
        t = a(:,j) * 2
        a(1,j) = t(1)*0.5_dp + eps
        s = s + t(1 + mod(i, size(t)))
    end do
end do
end subroutine

! c = matmul(a, b)
subroutine k_matmul(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:,:), b(:,:), c(:,:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1,1) = a(1,1) + eps
    c = matmul(a, b)
    s = s + c(1 + mod(i, size(c, 1)), 1)
end do
end subroutine

! c = a + b, rank 2
subroutine k_add2d(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:,:), b(:,:), c(:,:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1,1) = a(1,1) + eps
    c = a + b
    s = s + c(1 + mod(i, size(c, 1)), 1 + mod(i/size(c, 1), size(c, 2)))
end do
end subroutine

! c = pack(a, m); m has a fixed number of .true. elements, so c keeps its shape
subroutine k_pack(a, m, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
logical, allocatable, intent(in) :: m(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = pack(a, m)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! st%x = st%x + dt*st%v, allocatable component on both sides
subroutine k_comp(st, reps, s)
type(state_t), intent(inout) :: st
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    st%x = st%x + eps*st%v
    s = s + st%x(1 + mod(i, size(st%x)))
end do
end subroutine

! q%x = p%x + 2*p%v, allocatable component of another object
subroutine k_dtexpr(p, q, reps, s)
type(state_t), intent(inout) :: p, q
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    p%x(1) = p%x(1) + eps
    q%x = p%x + 2*p%v
    s = s + q%x(1 + mod(i, size(q%x)))
end do
end subroutine

subroutine axpy_inout(x, v, dt)
real(dp), allocatable, intent(inout) :: x(:)
real(dp), intent(in) :: v(:), dt
x = x + dt*v
end subroutine

! x = x + dt*v inside a routine with an allocatable intent(inout) dummy,
! called from the hot loop (the callee cannot see the allocation status)
subroutine k_inout(x, v, reps, s)
real(dp), allocatable, intent(inout) :: x(:), v(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    call axpy_inout(x, v, eps)
    s = s + x(1 + mod(i, size(x)))
end do
end subroutine

! un = u; un(2:n-1) = ...; u = un
subroutine k_stencil(u, un, reps, s)
real(dp), allocatable, intent(inout) :: u(:), un(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, n
n = size(u)
s = 0
do i = 1, reps
    un = u
    un(2:n-1) = u(2:n-1) + 0.1_dp*(u(1:n-2) - 2*u(2:n-1) + u(3:n))
    u = un
    s = s + u(1 + mod(i, n))
end do
end subroutine

! Requires reallocation: x = [x, v] grows x by one element per iteration;
! x is reset to size 0 whenever it reaches nmax elements.
subroutine k_append(a, nmax, reps, s)
real(dp), allocatable, intent(inout) :: a(:)
integer, intent(in) :: nmax, reps
real(dp), intent(out) :: s
real(dp), allocatable :: x(:)
integer :: i
allocate(x(0))
s = 0
do i = 1, reps
    if (size(x) >= nmax) then
        deallocate(x)
        allocate(x(0))
    end if
    x = [x, a(1 + mod(i, size(a)))]
    s = s + x(size(x)) + x(1 + mod(i, size(x)))
end do
end subroutine

subroutine use_local(a, i, r)
real(dp), intent(in) :: a(:)
integer, intent(in) :: i
real(dp), intent(out) :: r
real(dp), allocatable :: t(:)
t = 2*a + 1
r = t(1 + mod(i, size(t)))
end subroutine

! Requires reallocation: assignment to an unallocated local allocatable
! inside a called routine (allocated on every call, freed on return)
subroutine k_local(a, reps, s)
real(dp), allocatable, intent(inout) :: a(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
real(dp) :: r
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call use_local(a, i, r)
    s = s + r
end do
end subroutine

! Requires reallocation: the shape of the right-hand side changes on every
! iteration (between n - n/2 and n elements)
subroutine k_resize(a, x, reps, s)
real(dp), allocatable, intent(inout) :: a(:), x(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, n, m
n = size(a)
s = 0
do i = 1, reps
    m = n - mod(i, n/2 + 1)
    a(1) = a(1) + eps
    x = a(1:m) + 1
    s = s + x(m) + x(1 + mod(i, m))
end do
end subroutine

end module

program realloc_lhs
use realloc_kernels
implicit none
character(len=32) :: name, arg
integer :: n, reps
real(dp), allocatable :: a(:), b(:), c(:), a2(:,:), b2(:,:), c2(:,:)
logical, allocatable :: m(:)
type(state_t) :: st, q
real(dp) :: s, t
integer(8) :: t0, t1, rate
integer :: i
if (command_argument_count() /= 3) error stop "usage: realloc_lhs <kernel> <n> <reps>"
call get_command_argument(1, name)
call get_command_argument(2, arg)
read(arg, *) n
call get_command_argument(3, arg)
read(arg, *) reps
if (n < 3) error stop "n must be at least 3"
allocate(a(n), b(n), c(n))
do i = 1, n
    a(i) = i
    b(i) = 0.5_dp*i
end do
c = 0
s = 0
select case (trim(name))
case ("ctor")
    deallocate(c)
    allocate(c(3))
    c = 0
case ("section")
    allocate(a2(n,16))
    a2 = 1
case ("matmul", "add2d")
    allocate(a2(n,n), b2(n,n), c2(n,n))
    a2 = 1
    b2 = 0.5_dp
    c2 = 0
    do i = 1, n
        a2(i,1) = i
    end do
case ("pack")
    allocate(m(n))
    do i = 1, n
        m(i) = mod(i, 2) == 1
    end do
    deallocate(c)
    allocate(c(count(m)))
    c = 0
case ("comp")
    allocate(st%x(n), st%v(n))
    st%x = a
    st%v = b
case ("dtexpr")
    allocate(st%x(n), st%v(n), q%x(n))
    st%x = a
    st%v = b
    q%x = 0
end select
call system_clock(t0, rate)
select case (trim(name))
case ("add")
    call k_add(a, b, c, reps, s)
case ("self")
    call k_self(a, b, reps, s)
case ("copy")
    call k_copy(a, c, reps, s)
case ("ctor")
    call k_ctor(a, c, reps, s)
case ("func")
    call k_func(a, c, reps, s)
case ("allocfn")
    call k_allocfn(a, c, reps, s)
case ("section")
    call k_section(a2, c, reps, s)
case ("matmul")
    call k_matmul(a2, b2, c2, reps, s)
case ("add2d")
    call k_add2d(a2, b2, c2, reps, s)
case ("pack")
    call k_pack(a, m, c, reps, s)
case ("comp")
    call k_comp(st, reps, s)
case ("dtexpr")
    call k_dtexpr(st, q, reps, s)
case ("inout")
    call k_inout(a, b, reps, s)
case ("stencil")
    call k_stencil(a, c, reps, s)
case ("append")
    call k_append(a, n, reps, s)
case ("local")
    call k_local(a, reps, s)
case ("resize")
    call k_resize(a, c, reps, s)
case default
    error stop "unknown kernel"
end select
call system_clock(t1)
t = real(t1 - t0, dp) / real(rate, dp)
print "(a, 1x, i0, 1x, i0, 1x, es23.16, 1x, es24.16)", trim(name), n, reps, t, s
end program
