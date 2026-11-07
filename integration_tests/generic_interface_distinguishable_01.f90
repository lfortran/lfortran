! Generics whose specific procedures are distinguishable (F2018 C1514) by
! kind, rank, type, argument count or position, ALLOCATABLE versus POINTER,
! derived type, or the number of dummy arguments a polymorphic dummy argument
! of the other specific accepts; also type-bound generics with PASS, NOPASS
! and overriding. Each reference must resolve to the expected specific.
module generic_interface_distinguishable_01_m
    implicit none
    integer :: which = 0

    type :: t1
    end type
    type :: t2
    end type
    type :: base
    end type
    type, extends(base) :: ext_a
    end type
    type, extends(base) :: ext_b
    end type

    type :: pt
    contains
        procedure :: p_int
        procedure, nopass :: np_real
        generic :: g => p_int, np_real
    end type
    type, extends(pt) :: pt_child
    contains
        procedure :: p_int => child_p_int
        procedure :: p_vec
        generic :: g => p_vec
    end type

    interface g_kind_rank
        module procedure k4, k8, r1
    end interface
    interface g_type
        module procedure ti, tch, tl
    end interface
    interface g_count
        module procedure n1, n2
    end interface
    interface g_position
        module procedure pos_a, pos_b
    end interface
    interface g_alloc_ptr
        module procedure ap_alloc, ap_ptr
    end interface
    interface g_derived
        module procedure dt1, dt2
    end interface
    ! cc2's class(base) dummy is TKR compatible with both of cc1's, but cc2
    ! has only one dummy argument not distinguishable from it (rule (1)).
    interface g_poly_count
        module procedure cc1, cc2
    end interface
    interface g_optional
        module procedure op_a, op_b
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
    subroutine r1(x)
        real(4), intent(in) :: x(:)
        which = 3
    end subroutine

    subroutine ti(x)
        integer, intent(in) :: x
        which = 4
    end subroutine
    subroutine tch(x)
        character(len=*), intent(in) :: x
        which = 5
    end subroutine
    subroutine tl(x)
        logical, intent(in) :: x
        which = 6
    end subroutine

    subroutine n1(x)
        real, intent(in) :: x
        which = 7
    end subroutine
    subroutine n2(x, y)
        real, intent(in) :: x, y
        which = 8
    end subroutine

    subroutine pos_a(x, y)
        real, intent(in) :: x
        integer, intent(in) :: y
        which = 9
    end subroutine
    subroutine pos_b(z, x)
        integer, intent(in) :: z
        real, intent(in) :: x
        which = 10
    end subroutine

    subroutine ap_alloc(x)
        real, allocatable, intent(inout) :: x(:)
        which = 11
    end subroutine
    subroutine ap_ptr(x)
        real, pointer, intent(inout) :: x(:)
        which = 12
    end subroutine

    subroutine dt1(a)
        type(t1), intent(in) :: a
        which = 13
    end subroutine
    subroutine dt2(a)
        class(t2), intent(in) :: a
        which = 14
    end subroutine

    subroutine cc1(a, b)
        type(ext_a), intent(in) :: a
        type(ext_b), intent(in) :: b
        which = 15
    end subroutine
    subroutine cc2(x)
        class(base), intent(in) :: x
        which = 16
    end subroutine

    subroutine op_a(x, n)
        real, intent(in) :: x
        integer, intent(in), optional :: n
        which = 17
    end subroutine
    subroutine op_b(n)
        integer, intent(in) :: n
        which = 18
    end subroutine

    subroutine p_int(self, n)
        class(pt), intent(in) :: self
        integer, intent(in) :: n
        which = 19
    end subroutine
    subroutine np_real(r)
        real, intent(in) :: r
        which = 20
    end subroutine
    subroutine child_p_int(self, n)
        class(pt_child), intent(in) :: self
        integer, intent(in) :: n
        which = 21
    end subroutine
    subroutine p_vec(self, v)
        class(pt_child), intent(in) :: self
        integer, intent(in) :: v(:)
        which = 22
    end subroutine
end module

program generic_interface_distinguishable_01
    use generic_interface_distinguishable_01_m
    implicit none
    real, allocatable :: xa(:)
    type(t1) :: a1
    type(t2) :: a2
    type(ext_a) :: ea
    type(ext_b) :: eb
    type(pt) :: p
    type(pt_child) :: pc

    call g_kind_rank(1.0_4); if (which /= 1) error stop
    call g_kind_rank(1.0_8); if (which /= 2) error stop
    call g_kind_rank([1.0_4]); if (which /= 3) error stop
    call g_type(1); if (which /= 4) error stop
    call g_type("a"); if (which /= 5) error stop
    call g_type(.true.); if (which /= 6) error stop
    call g_count(1.0); if (which /= 7) error stop
    call g_count(1.0, 2.0); if (which /= 8) error stop
    call g_position(1.0, 2); if (which /= 9) error stop
    call g_position(2, 1.0); if (which /= 10) error stop
    allocate(xa(1))
    call g_alloc_ptr(xa); if (which /= 11) error stop
    call g_derived(a1); if (which /= 13) error stop
    call g_derived(a2); if (which /= 14) error stop
    call g_poly_count(ea, eb); if (which /= 15) error stop
    call g_poly_count(ea); if (which /= 16) error stop
    call g_optional(1.0); if (which /= 17) error stop
    call g_optional(1.0, 2); if (which /= 17) error stop
    call g_optional(2); if (which /= 18) error stop
    call p%g(1); if (which /= 19) error stop
    call p%g(1.0); if (which /= 20) error stop
    call pc%g(1); if (which /= 21) error stop
    call pc%g([1, 2]); if (which /= 22) error stop
    call pc%g(1.0); if (which /= 20) error stop
    print *, "ok"
end program
