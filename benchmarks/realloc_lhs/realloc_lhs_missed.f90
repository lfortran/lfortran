! Kernels for the optimizations that the compiler declines, or does in a
! weaker form, because an assignment may reallocate its target: not the
! check itself, but a temporary, a copy or a heap allocation that exists only
! because of it (see README.md). Each kernel comes in two forms: the plain
! assignment, and a rewrite (its name ends in `_x`) that cannot reallocate:
! `x(:) = ...` to an allocated target, or an explicit ALLOCATE. The targets
! of the plain forms are allocated with the shape of the value before the
! loop, so that they are valid without reallocation on assignment too,
! except for `promote`, whose work array is allocated by its assignment.
!
! Usage: realloc_lhs_missed <kernel> <n> <reps>
!
! Prints one line: "<kernel> <n> <reps> <seconds> <checksum>", as
! realloc_lhs does. The two forms of a kernel print the same checksum.
module realloc_missed_kernels
implicit none

! In realloc_lhs_ext.f90: an opaque call, and the placement padding of
! run_benchmark.py
interface
    subroutine ext_tick(k)
    integer, intent(inout) :: k
    end subroutine
end interface

type :: box_t
    real, allocatable :: v(:)
end type

contains

! An explicit-shape function result
pure function f3(a) result(r)
    real, intent(in) :: a(:)
    real :: r(size(a))
    r = 2*a + 1
end function

! The same with logical elements
pure function g3(a) result(r)
    real, intent(in) :: a(:)
    logical :: r(size(a))
    r = a > 1
end function

! fres_comp: an explicit-shape result assigned to an allocatable component.
! Without reallocation the result is computed into the component; with it,
! into a temporary that is then copied.
subroutine k_fres_comp(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(box_t) :: b
    real :: a(n)
    integer :: i
    a = 1
    allocate(b%v(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        b%v = f3(a)
        s = s + b%v(1)
    end do
end subroutine

subroutine k_fres_comp_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    type(box_t) :: b
    real :: a(n)
    integer :: i
    a = 1
    allocate(b%v(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        b%v(:) = f3(a)
        s = s + b%v(1)
    end do
end subroutine

! fres_internal: the same to a local of a procedure with an internal
! procedure
subroutine k_fres_internal(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c = f3(a)
        call add_first()
    end do
contains
    subroutine add_first()
        s = s + c(1)
    end subroutine
end subroutine

subroutine k_fres_internal_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c(:) = f3(a)
        call add_first()
    end do
contains
    subroutine add_first()
        s = s + c(1)
    end subroutine
end subroutine

! fres_logical: an explicit-shape logical result to a local
subroutine k_fres_logical(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    logical, allocatable :: m(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(m(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        m = g3(a)
        if (m(1)) s = s + 1
    end do
end subroutine

subroutine k_fres_logical_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    logical, allocatable :: m(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(m(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        m(:) = g3(a)
        if (m(1)) s = s + 1
    end do
end subroutine

! aconst: an array constant (n is 3)
subroutine k_aconst(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    integer :: i
    allocate(c(n))
    s = 0
    do i = 1, reps
        c = [1., 2., 3.]
        c(1) = c(1) + real(mod(i, 7))
        s = s + c(1)
    end do
end subroutine

subroutine k_aconst_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    integer :: i
    allocate(c(n))
    s = 0
    do i = 1, reps
        c(:) = [1., 2., 3.]
        c(1) = c(1) + real(mod(i, 7))
        s = s + c(1)
    end do
end subroutine

! actor: an array constructor of expressions (n is 3)
subroutine k_actor(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c = [a(1) + 1, a(2)*2, a(3)]
        s = s + c(1)
    end do
end subroutine

subroutine k_actor_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c(:) = [a(1) + 1, a(2)*2, a(3)]
        s = s + c(1)
    end do
end subroutine

! aidl: an implied-DO array constructor
subroutine k_aidl(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i, j
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c = [(a(j)*j, j = 1, n)]
        s = s + c(1)
    end do
end subroutine

subroutine k_aidl_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i, j
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c(:) = [(a(j)*j, j = 1, n)]
        s = s + c(1)
    end do
end subroutine

! cshift: an intrinsic that gives a new array
subroutine k_cshift(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c = cshift(a, 1)
        s = s + c(n)
    end do
end subroutine

subroutine k_cshift_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c(:) = cshift(a, 1)
        s = s + c(n)
    end do
end subroutine

! transpose (n x n)
subroutine k_transpose(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:,:)
    real :: m(n, n)
    integer :: i
    m = 1
    allocate(c(n, n))
    s = 0
    do i = 1, reps
        m(2, 1) = real(mod(i, 7))
        c = transpose(m)
        s = s + c(1, 2)
    end do
end subroutine

subroutine k_transpose_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:,:)
    real :: m(n, n)
    integer :: i
    m = 1
    allocate(c(n, n))
    s = 0
    do i = 1, reps
        m(2, 1) = real(mod(i, 7))
        c(:,:) = transpose(m)
        s = s + c(1, 2)
    end do
end subroutine

! reshape (2 x 2, n is 4). The source has a constant size: of an array
! whose size is known only at run time, LFortran puts a temporary on the
! stack in every iteration and runs out of stack after about a million
! iterations.
subroutine k_reshape(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:,:)
    real :: a(4)
    integer :: i
    a = 1
    allocate(c(2, 2))
    s = 0
    do i = 1, reps
        a(3) = real(mod(i, 7))
        c = reshape(a, [2, 2])
        s = s + c(1, 2)
    end do
end subroutine

subroutine k_reshape_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:,:)
    real :: a(4)
    integer :: i
    a = 1
    allocate(c(2, 2))
    s = 0
    do i = 1, reps
        a(3) = real(mod(i, 7))
        c(:,:) = reshape(a, [2, 2])
        s = s + c(1, 2)
    end do
end subroutine

! pack, with a mask that keeps every element
subroutine k_pack(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c = pack(a, a > -1e30)
        s = s + c(1)
    end do
end subroutine

subroutine k_pack_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        a(1) = real(mod(i, 7))
        c(:) = pack(a, a > -1e30)
        s = s + c(1)
    end do
end subroutine

! matmul (n x n times n): computed into the target in both forms, a control
subroutine k_matmul(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: m(n, n), v(n)
    integer :: i
    m = 1
    v = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        v(1) = real(mod(i, 7))
        c = matmul(m, v)
        s = s + c(1)
    end do
end subroutine

subroutine k_matmul_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real, allocatable :: c(:)
    real :: m(n, n), v(n)
    integer :: i
    m = 1
    v = 1
    allocate(c(n))
    s = 0
    do i = 1, reps
        v(1) = real(mod(i, 7))
        c(:) = matmul(m, v)
        s = s + c(1)
    end do
end subroutine

! promote: a work array of a procedure, of a size known on entry, assigned
! twice around an opaque call. The first assignment allocates it (it is
! certainly unallocated there), the second keeps a reallocation, which
! keeps the array on the heap under --fast; allocated explicitly, it becomes
! an array of the procedure's frame.
subroutine work8(a, k, s)
    real, intent(in) :: a(8)
    integer, intent(inout) :: k
    real(8), intent(inout) :: s
    real, allocatable :: t(:)
    t = 2*a
    s = s + t(1)
    call ext_tick(k)
    t = 3*a
    s = s + t(2)
end subroutine

subroutine work8_x(a, k, s)
    real, intent(in) :: a(8)
    integer, intent(inout) :: k
    real(8), intent(inout) :: s
    real, allocatable :: t(:)
    allocate(t(8))
    t(:) = 2*a
    s = s + t(1)
    call ext_tick(k)
    t(:) = 3*a
    s = s + t(2)
end subroutine

subroutine k_promote(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real :: a(8)
    integer :: i, k
    a = real(n)
    k = 0
    s = 0
    do i = 1, reps
        a(2) = real(mod(i, 7))
        call work8(a, k, s)
    end do
end subroutine

subroutine k_promote_x(n, reps, s)
    integer, intent(in) :: n, reps
    real(8), intent(out) :: s
    real :: a(8)
    integer :: i, k
    a = real(n)
    k = 0
    s = 0
    do i = 1, reps
        a(2) = real(mod(i, 7))
        call work8_x(a, k, s)
    end do
end subroutine

end module

program realloc_lhs_missed
use realloc_missed_kernels
implicit none
character(len=32) :: name, arg
integer :: n, reps
integer(8) :: t0, t1, rate
real(8) :: s, t

call get_command_argument(1, name)
call get_command_argument(2, arg)
read (arg, *) n
call get_command_argument(3, arg)
read (arg, *) reps
call system_clock(t0, rate)
select case (trim(name))
case ("fres_comp")
    call k_fres_comp(n, reps, s)
case ("fres_comp_x")
    call k_fres_comp_x(n, reps, s)
case ("fres_internal")
    call k_fres_internal(n, reps, s)
case ("fres_internal_x")
    call k_fres_internal_x(n, reps, s)
case ("fres_logical")
    call k_fres_logical(n, reps, s)
case ("fres_logical_x")
    call k_fres_logical_x(n, reps, s)
case ("aconst")
    call k_aconst(n, reps, s)
case ("aconst_x")
    call k_aconst_x(n, reps, s)
case ("actor")
    call k_actor(n, reps, s)
case ("actor_x")
    call k_actor_x(n, reps, s)
case ("aidl")
    call k_aidl(n, reps, s)
case ("aidl_x")
    call k_aidl_x(n, reps, s)
case ("cshift")
    call k_cshift(n, reps, s)
case ("cshift_x")
    call k_cshift_x(n, reps, s)
case ("transpose")
    call k_transpose(n, reps, s)
case ("transpose_x")
    call k_transpose_x(n, reps, s)
case ("reshape")
    call k_reshape(n, reps, s)
case ("reshape_x")
    call k_reshape_x(n, reps, s)
case ("pack")
    call k_pack(n, reps, s)
case ("pack_x")
    call k_pack_x(n, reps, s)
case ("matmul")
    call k_matmul(n, reps, s)
case ("matmul_x")
    call k_matmul_x(n, reps, s)
case ("promote")
    call k_promote(n, reps, s)
case ("promote_x")
    call k_promote_x(n, reps, s)
case default
    error stop "unknown kernel"
end select
call system_clock(t1)
t = real(t1 - t0, 8) / real(rate, 8)
print "(a, 1x, i0, 1x, i0, 1x, es23.16, 1x, es24.16)", trim(name), n, reps, t, s
end program
