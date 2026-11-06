! Generic interfaces whose specific procedures are distinguishable
! (F2018 C1514) by kind, rank, type, argument count, argument names,
! ALLOCATABLE versus POINTER, procedure versus data object, derived type,
! optional arguments or the number of arguments a polymorphic dummy argument
! of the other specific accepts, also with TYPE(*), CLASS(*) and assumed-rank
! dummy arguments. Each call must resolve to the expected
! specific.
module generic_interface_distinguishable_01_m
    implicit none
    integer :: which = 0

    type :: t1
        integer :: i = 1
    end type
    type :: t2
        integer :: i = 2
    end type
    type :: base
        integer :: i = 3
    end type
    type :: other
        integer :: i = 4
    end type
    type :: cnt_base
        integer :: i = 5
    end type
    type, extends(cnt_base) :: cnt_a
    end type
    type, extends(cnt_base) :: cnt_b
    end type

    abstract interface
        real function real_fun(x)
            real, intent(in) :: x
        end function
    end interface

    interface g_kind
        module procedure k4, k8
    end interface

    interface g_rank
        module procedure r0, r1, r2
    end interface

    interface g_type
        module procedure ti, tr, tc, tl, tch
    end interface

    interface g_nargs
        module procedure n1, n2
    end interface

    interface g_name
        module procedure nm_a, nm_b
    end interface

    interface g_alloc_ptr
        module procedure ap_alloc, ap_ptr
    end interface

    interface g_proc_data
        module procedure pd_proc, pd_data
    end interface

    interface g_derived
        module procedure dt1, dt2
    end interface

    interface g_class
        module procedure cl_base, cl_other
    end interface

    interface g_optional
        module procedure op_a, op_b
    end interface

    interface g_fun
        module procedure fi, fr
    end interface

    ! The class(*) dummy of cs2 is TKR compatible with both nonoptional
    ! dummy arguments of cs1, but only one dummy argument of cs2 is not
    ! distinguishable from it (C1514 (1)).
    interface g_count_star
        module procedure cs1, cs2
    end interface

    ! The same with a class(cnt_base) dummy and two extensions of cnt_base.
    interface g_count_class
        module procedure cc1, cc2
    end interface

    ! TYPE(*) scalar and integer rank 1: ranks differ.
    interface g_star_rank
        module procedure sr1, sr2
    end interface

    ! Assumed rank and rank 2 of different types: types differ.
    interface g_arank
        module procedure ar1, ar2
    end interface

    ! CLASS(*) rank 1 and real scalar: ranks differ.
    interface g_star_r1
        module procedure su1, su2
    end interface

    ! TYPE(*) only, with one and two nonoptional dummy arguments (C1514 (1)).
    interface g_star_count
        module procedure sn1, sn2
    end interface

    interface g_body
        subroutine gid01_ext_i(n)
            integer, intent(in) :: n
        end subroutine
        subroutine gid01_ext_r(x)
            real, intent(in) :: x
        end subroutine
    end interface

contains

    subroutine k4(x)
        real(4), intent(in) :: x
        which = 1
    end subroutine
    subroutine k8(x)
        real(8), intent(in) :: x
        which = 2
    end subroutine

    subroutine r0(x)
        integer, intent(in) :: x
        which = 3
    end subroutine
    subroutine r1(x)
        integer, intent(in) :: x(:)
        which = 4
    end subroutine
    subroutine r2(x)
        integer, intent(in) :: x(:, :)
        which = 5
    end subroutine

    subroutine ti(x)
        integer, intent(in) :: x
        which = 6
    end subroutine
    subroutine tr(x)
        real, intent(in) :: x
        which = 7
    end subroutine
    subroutine tc(x)
        complex, intent(in) :: x
        which = 8
    end subroutine
    subroutine tl(x)
        logical, intent(in) :: x
        which = 9
    end subroutine
    subroutine tch(x)
        character(len=*), intent(in) :: x
        which = 10
    end subroutine

    subroutine n1(x)
        real, intent(in) :: x
        which = 11
    end subroutine
    subroutine n2(x, y)
        real, intent(in) :: x, y
        which = 12
    end subroutine

    ! Same types at each position for some calls, told apart by names:
    ! nm_a has a nonoptional y, which nm_b does not have.
    subroutine nm_a(x, y)
        real, intent(in) :: x
        integer, intent(in) :: y
        which = 13
    end subroutine
    subroutine nm_b(z, x)
        integer, intent(in) :: z
        real, intent(in) :: x
        which = 14
    end subroutine

    subroutine ap_alloc(x)
        real, allocatable, intent(inout) :: x(:)
        which = 15
    end subroutine
    subroutine ap_ptr(x)
        real, pointer, intent(inout) :: x(:)
        which = 16
    end subroutine

    subroutine pd_proc(f)
        procedure(real_fun) :: f
        if (abs(f(2.0) - 4.0) > 1e-6) error stop
        which = 17
    end subroutine
    subroutine pd_data(x)
        real, intent(in) :: x
        which = 18
    end subroutine

    subroutine dt1(a)
        type(t1), intent(in) :: a
        which = 19
    end subroutine
    subroutine dt2(a)
        type(t2), intent(in) :: a
        which = 20
    end subroutine

    subroutine cl_base(a)
        class(base), intent(in) :: a
        which = 21
    end subroutine
    subroutine cl_other(a)
        type(other), intent(in) :: a
        which = 22
    end subroutine

    subroutine op_a(x, y)
        real, intent(in) :: x
        integer, intent(in), optional :: y
        which = 23
    end subroutine
    subroutine op_b(x)
        integer, intent(in) :: x
        which = 24
    end subroutine

    integer function fi(x)
        integer, intent(in) :: x
        fi = 25 + 0 * x
    end function
    integer function fr(x)
        real, intent(in) :: x
        fr = 26 + 0 * int(x)
    end function

    subroutine cs1(a, x)
        integer, intent(in) :: a
        real, intent(in) :: x
        which = 29 + 0 * a + 0 * int(x)
    end subroutine
    subroutine cs2(x)
        class(*), intent(in) :: x
        which = 30
    end subroutine

    subroutine cc1(a, x)
        type(cnt_a), intent(in) :: a
        type(cnt_b), intent(in) :: x
        which = 31 + 0 * a%i + 0 * x%i
    end subroutine
    subroutine cc2(x)
        class(cnt_base), intent(in) :: x
        which = 32 + 0 * x%i
    end subroutine

    subroutine sr1(x)
        type(*), intent(in) :: x
        which = 33
    end subroutine
    subroutine sr2(x)
        integer, intent(in) :: x(:)
        which = 34 + 0 * size(x)
    end subroutine

    subroutine ar1(x)
        integer, intent(in) :: x(..)
        which = 35 + 0 * rank(x)
    end subroutine
    subroutine ar2(x)
        real, intent(in) :: x(:,:)
        which = 36 + 0 * size(x)
    end subroutine

    subroutine su1(x)
        class(*), intent(in) :: x(:)
        which = 37 + 0 * size(x)
    end subroutine
    subroutine su2(x)
        real, intent(in) :: x
        which = 38 + 0 * int(x)
    end subroutine

    subroutine sn1(x)
        type(*), intent(in) :: x
        which = 39
    end subroutine
    subroutine sn2(x, y)
        type(*), intent(in) :: x, y
        which = 40
    end subroutine

    real function twice(x)
        real, intent(in) :: x
        twice = 2 * x
    end function

end module

subroutine gid01_ext_i(n)
    use generic_interface_distinguishable_01_m, only: which
    integer, intent(in) :: n
    which = 27 + 0 * n
end subroutine

subroutine gid01_ext_r(x)
    use generic_interface_distinguishable_01_m, only: which
    real, intent(in) :: x
    which = 28 + 0 * int(x)
end subroutine

program generic_interface_distinguishable_01
    use generic_interface_distinguishable_01_m
    use, intrinsic :: ieee_arithmetic, only: ieee_support_denormal, &
        ieee_support_rounding, ieee_nearest
    implicit none
    real, allocatable :: xa(:)
    type(t1) :: a1
    type(t2) :: a2
    type(base) :: b
    type(other) :: o
    integer :: i2(2, 2)
    type(cnt_a) :: ca
    type(cnt_b) :: cb
    real :: ra2(2, 2)

    call g_kind(1.0_4)
    if (which /= 1) error stop
    call g_kind(1.0_8)
    if (which /= 2) error stop

    call g_rank(1)
    if (which /= 3) error stop
    call g_rank([1, 2])
    if (which /= 4) error stop
    i2 = 0
    call g_rank(i2)
    if (which /= 5) error stop

    call g_type(1)
    if (which /= 6) error stop
    call g_type(1.0)
    if (which /= 7) error stop
    call g_type((1.0, 2.0))
    if (which /= 8) error stop
    call g_type(.true.)
    if (which /= 9) error stop
    call g_type("abc")
    if (which /= 10) error stop

    call g_nargs(1.0)
    if (which /= 11) error stop
    call g_nargs(1.0, 2.0)
    if (which /= 12) error stop

    call g_name(1.0, 2)
    if (which /= 13) error stop
    call g_name(2, 1.0)
    if (which /= 14) error stop
    call g_name(x=1.0, y=2)
    if (which /= 13) error stop
    call g_name(z=2, x=1.0)
    if (which /= 14) error stop

    allocate(xa(2))
    call g_alloc_ptr(xa)
    if (which /= 15) error stop

    call g_proc_data(twice)
    if (which /= 17) error stop
    call g_proc_data(1.0)
    if (which /= 18) error stop

    call g_derived(a1)
    if (which /= 19) error stop
    call g_derived(a2)
    if (which /= 20) error stop

    call g_class(b)
    if (which /= 21) error stop
    call g_class(o)
    if (which /= 22) error stop

    call g_optional(1.0)
    if (which /= 23) error stop
    call g_optional(1.0, 2)
    if (which /= 23) error stop
    call g_optional(2)
    if (which /= 24) error stop

    if (g_fun(1) /= 25) error stop
    if (g_fun(1.0) /= 26) error stop

    call g_body(1)
    if (which /= 27) error stop
    call g_body(1.0)
    if (which /= 28) error stop

    call g_count_star(1, 2.0)
    if (which /= 29) error stop
    call g_count_star(3)
    if (which /= 30) error stop
    call g_count_star(4.0)
    if (which /= 30) error stop

    call g_count_class(ca, cb)
    if (which /= 31) error stop
    call g_count_class(ca)
    if (which /= 32) error stop
    call g_count_class(cb)
    if (which /= 32) error stop

    call g_star_rank(1.0)
    if (which /= 33) error stop
    call g_star_rank([1, 2])
    if (which /= 34) error stop

    call g_arank(i2)
    if (which /= 35) error stop
    call g_arank(1)
    if (which /= 35) error stop
    ra2 = 0
    call g_arank(ra2)
    if (which /= 36) error stop

    call g_star_r1([1, 2])
    if (which /= 37) error stop
    call g_star_r1(1.0)
    if (which /= 38) error stop

    call g_star_count(1)
    if (which /= 39) error stop
    call g_star_count(1, 2.0)
    if (which /= 40) error stop

    ! The generic IEEE inquiries of the runtime take an optional X.
    if (.not. ieee_support_denormal()) error stop
    if (.not. ieee_support_denormal(1.0)) error stop
    if (.not. ieee_support_denormal(1.0d0)) error stop
    if (.not. ieee_support_rounding(ieee_nearest)) error stop
    if (.not. ieee_support_rounding(ieee_nearest, 1.0d0)) error stop

    print *, "ok"
end program
