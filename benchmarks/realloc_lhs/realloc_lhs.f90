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

! The procedures of realloc_lhs_ext.f90, compiled separately
interface
    subroutine ext_tick(k)
    integer, intent(inout) :: k
    end subroutine
    subroutine ext_add(a, b, c)
    import :: dp
    real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
    end subroutine
    subroutine ext_add2d(a, b, c)
    import :: dp
    real(dp), allocatable, intent(inout) :: a(:,:), b(:,:), c(:,:)
    end subroutine
    subroutine ext_add3d(a, b, c)
    import :: dp
    real(dp), allocatable, intent(inout) :: a(:,:,:), b(:,:,:), c(:,:,:)
    end subroutine
    subroutine ext_chain(a, b, t1, t2, c)
    import :: dp
    real(dp), intent(in) :: a(:), b(:)
    real(dp), allocatable, intent(inout) :: t1(:), t2(:), c(:)
    end subroutine
    subroutine ext_chain_alloc(a, b, c)
    import :: dp
    real(dp), intent(in) :: a(:), b(:)
    real(dp), allocatable, intent(inout) :: c(:)
    end subroutine
    subroutine ext_chain_local(a, b, c)
    import :: dp
    real(dp), intent(in) :: a(:), b(:)
    real(dp), allocatable, intent(inout) :: c(:)
    end subroutine
end interface

type :: state_t
    real(dp), allocatable :: x(:), v(:)
end type

! A particle with 3-vector components
type :: particle_t
    real(dp), allocatable :: r(:), v(:), f(:), d(:)
end type

! A row of a jagged array
type :: row_t
    real(dp), allocatable :: v(:)
end type

! The target of `k_modvar`
real(dp), allocatable :: mc(:)

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

! The kernels below defeat the static elision and the hoisting of the shape
! check out of the loop: each says why. Except for those that require
! reallocation (alternate, xchainlocal), the left-hand side already has the
! right shape.

! c = a + b in a procedure of another file, called from the loop: the check
! runs in every call
subroutine k_xadd(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call ext_add(a, b, c)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! c = a + b, rank 2, in a procedure of another file
subroutine k_xadd2d(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:,:), b(:,:), c(:,:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1,1) = a(1,1) + eps
    call ext_add2d(a, b, c)
    s = s + c(1 + mod(i, size(c, 1)), 1 + mod(i/size(c, 1), size(c, 2)))
end do
end subroutine

! c = a + b, rank 3, in a procedure of another file
subroutine k_xadd3d(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:,:,:), b(:,:,:), c(:,:,:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1,1,1) = a(1,1,1) + eps
    call ext_add3d(a, b, c)
    s = s + c(1 + mod(i, size(c, 1)), 1 + mod(i/size(c, 1), size(c, 2)), 1)
end do
end subroutine

! t1 = a + b; t2 = t1*a; c = t2 - b in a procedure of another file, with
! allocatable work arrays passed in
subroutine k_xchain(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
real(dp), allocatable :: t1(:), t2(:)
integer :: i
allocate(t1(size(a)), t2(size(a)))
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call ext_chain(a, b, t1, t2, c)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! The same with local work arrays the procedure allocates explicitly
subroutine k_xchainalloc(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call ext_chain_alloc(a, b, c)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! Requires reallocation: the same with local work arrays that the
! assignments allocate
subroutine k_xchainlocal(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call ext_chain_local(a, b, c)
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! An opaque call before c = a + b: the check cannot move before the call
subroutine k_opaque(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, k
s = 0
k = 0
do i = 1, reps
    call ext_tick(k)
    a(1) = a(1) + eps
    c = a + b
    s = s + c(1 + mod(k, size(c)))
end do
end subroutine

! A test that can leave the loop before c = a + b
subroutine k_exit(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    if (s < 0) exit
    a(1) = a(1) + eps
    c = a + b
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! c = a + b to a target with the TARGET attribute
subroutine k_target(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:)
real(dp), allocatable, target, intent(inout) :: c(:)
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

! mc = a + b to a module variable, with an opaque call in the loop
subroutine k_modvar(a, b, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, k
s = 0
k = 0
do i = 1, reps
    a(1) = a(1) + eps
    mc = a + b
    call ext_tick(k)
    s = s + mc(1 + mod(k, size(mc)))
end do
end subroutine

! c = a + b to host-associated variables in an internal procedure called
! from the loop
subroutine k_internal(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call step()
    s = s + c(1 + mod(i, size(c)))
end do
contains
subroutine step()
c = a + b
end subroutine
end subroutine

! A work array of a procedure kept between calls (SAVE), allocated on the
! first call, called from the loop
subroutine use_saved(a, b, i, r)
real(dp), intent(in) :: a(:), b(:)
integer, intent(in) :: i
real(dp), intent(out) :: r
real(dp), allocatable, save :: w(:)
if (.not. allocated(w)) allocate(w(size(a)))
w = a + b
r = w(1 + mod(i, size(w)))
end subroutine

subroutine k_saved(a, b, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
real(dp) :: r
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    call use_saved(a, b, i, r)
    s = s + r
end do
end subroutine

! d = p(j)%r - p(i)%r for all pairs of particles (3-vectors), to a local
! work array whose shape is read from p(j)
subroutine k_pairs(p, reps, s)
type(particle_t), intent(inout) :: p(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
real(dp), allocatable :: d(:)
integer :: it, i, j, np
np = size(p)
allocate(d(size(p(1)%r)))
s = 0
do it = 1, reps
    do i = 1, np
        do j = 1, np
            d = p(j)%r - p(i)%r
            p(i)%f = p(i)%f + eps*d
        end do
    end do
    s = s + p(1 + mod(it, np))%f(1)
end do
end subroutine

! p(i)%d = p(j)%r - p(i)%r for all pairs: the target is a component of an
! array element
subroutine k_pairsc(p, reps, s)
type(particle_t), intent(inout) :: p(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: it, i, j, np
np = size(p)
s = 0
do it = 1, reps
    do i = 1, np
        do j = 1, np
            p(i)%d = p(j)%r - p(i)%r
            p(i)%f = p(i)%f + eps*p(i)%d
        end do
    end do
    s = s + p(1 + mod(it, np))%f(1)
end do
end subroutine

! p(i)%d = p(i)%r + eps*p(i)%v in a DO CONCURRENT loop over the particles
subroutine k_concur(p, reps, s)
type(particle_t), intent(inout) :: p(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: it, i, np
np = size(p)
s = 0
do it = 1, reps
    p(1)%r(1) = p(1)%r(1) + eps
    do concurrent (i = 1:np)
        p(i)%d = p(i)%r + eps*p(i)%v
    end do
    s = s + p(1 + mod(it, np))%d(1)
end do
end subroutine

! out(i)%v = w(i)*rows(i)%v over the rows of a jagged array
subroutine k_jagged(rows, out, w, reps, s)
type(row_t), intent(inout) :: rows(:), out(:)
real(dp), intent(in) :: w(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: it, i
s = 0
do it = 1, reps
    rows(1)%v(1) = rows(1)%v(1) + eps
    do i = 1, size(rows)
        out(i)%v = w(i)*rows(i)%v
    end do
    s = s + out(1 + mod(it, size(out)))%v(1)
end do
end subroutine

! c = a + b in a DO WHILE loop
subroutine k_while(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
i = 0
do while (i < reps)
    i = i + 1
    a(1) = a(1) + eps
    c = a + b
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! c = a + b or c = a - b, depending on the iteration
subroutine k_cond(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    if (mod(i, 2) == 0) then
        c = a + b
    else
        c = a - b
    end if
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! t1 = a + b; t2 = t1*a; c = t2 - b through local work arrays
subroutine k_chain(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
real(dp), allocatable :: t1(:), t2(:)
integer :: i
allocate(t1(size(a)), t2(size(a)))
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    t1 = a + b
    t2 = t1*a
    c = t2 - b
    s = s + c(1 + mod(i, size(c)))
end do
end subroutine

! c = a(1:k) + b(1:k) with k read from data in every iteration (always the
! same value in practice)
subroutine k_datadep(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, k, counts(4)
counts = size(a)
s = 0
do i = 1, reps
    k = counts(1 + mod(i, 4))
    a(1) = a(1) + eps
    c = a(1:k) + b(1:k)
    s = s + c(1 + mod(i, k))
end do
end subroutine

! Requires reallocation: c = a(1:k) + b(1:k) with k alternating between n
! and n - 1
subroutine k_alternate(a, b, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, k
s = 0
do i = 1, reps
    k = size(a) - mod(i, 2)
    a(1) = a(1) + eps
    c = a(1:k) + b(1:k)
    s = s + c(1 + mod(i, k))
end do
end subroutine

! c = [(2*a(j), j = 1, m)], an array constructor with an implied DO loop
subroutine k_ctorv(a, c, reps, s)
real(dp), allocatable, intent(inout) :: a(:), c(:)
integer, intent(in) :: reps
real(dp), intent(out) :: s
integer :: i, j, m
m = size(a)
s = 0
do i = 1, reps
    a(1) = a(1) + eps
    c = [(2*a(j), j = 1, m)]
    s = s + c(1 + mod(i, m))
end do
end subroutine

end module

program realloc_lhs
use realloc_kernels
implicit none
character(len=32) :: name, arg
integer :: n, reps
real(dp), allocatable :: a(:), b(:), c(:), a2(:,:), b2(:,:), c2(:,:)
real(dp), allocatable :: a3(:,:,:), b3(:,:,:), c3(:,:,:), w(:)
logical, allocatable :: m(:)
type(state_t) :: st, q
type(particle_t), allocatable :: p(:)
type(row_t), allocatable :: rows(:), out(:)
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
case ("matmul", "add2d", "xadd2d")
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
case ("xadd3d")
    allocate(a3(n,n,n), b3(n,n,n), c3(n,n,n))
    a3 = 1
    b3 = 0.5_dp
    c3 = 0
case ("modvar")
    allocate(mc(n))
    mc = 0
case ("pairs", "pairsc", "concur")
    allocate(p(n))
    do i = 1, n
        allocate(p(i)%r(3), p(i)%v(3), p(i)%f(3), p(i)%d(3))
        p(i)%r(1) = i
        p(i)%r(2) = 2*i
        p(i)%r(3) = 0.5_dp*i
        p(i)%v = 0.25_dp*i
        p(i)%f = 0
        p(i)%d = 0
    end do
case ("jagged")
    allocate(rows(16), out(16), w(16))
    do i = 1, 16
        allocate(rows(i)%v(n), out(i)%v(n))
        rows(i)%v = a
        out(i)%v = 0
        w(i) = 1 + 0.125_dp*i
    end do
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
case ("xadd")
    call k_xadd(a, b, c, reps, s)
case ("xadd2d")
    call k_xadd2d(a2, b2, c2, reps, s)
case ("xadd3d")
    call k_xadd3d(a3, b3, c3, reps, s)
case ("xchain")
    call k_xchain(a, b, c, reps, s)
case ("xchainalloc")
    call k_xchainalloc(a, b, c, reps, s)
case ("xchainlocal")
    call k_xchainlocal(a, b, c, reps, s)
case ("opaque")
    call k_opaque(a, b, c, reps, s)
case ("exit")
    call k_exit(a, b, c, reps, s)
case ("target")
    call k_target(a, b, c, reps, s)
case ("modvar")
    call k_modvar(a, b, reps, s)
case ("internal")
    call k_internal(a, b, c, reps, s)
case ("saved")
    call k_saved(a, b, reps, s)
case ("pairs")
    call k_pairs(p, reps, s)
case ("pairsc")
    call k_pairsc(p, reps, s)
case ("concur")
    call k_concur(p, reps, s)
case ("jagged")
    call k_jagged(rows, out, w, reps, s)
case ("while")
    call k_while(a, b, c, reps, s)
case ("cond")
    call k_cond(a, b, c, reps, s)
case ("chain")
    call k_chain(a, b, c, reps, s)
case ("datadep")
    call k_datadep(a, b, c, reps, s)
case ("alternate")
    call k_alternate(a, b, c, reps, s)
case ("ctorv")
    call k_ctorv(a, c, reps, s)
case default
    error stop "unknown kernel"
end select
call system_clock(t1)
t = real(t1 - t0, dp) / real(rate, dp)
print "(a, 1x, i0, 1x, i0, 1x, es23.16, 1x, es24.16)", trim(name), n, reps, t, s
end program
