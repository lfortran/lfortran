module conditional_arg_03_mod
    implicit none
    type :: base
        integer :: n
    end type
    type, extends(base) :: ext
        integer :: m
    end type
contains
    subroutine check_dynamic(v, expect_ext)
        class(base), intent(in) :: v
        logical, intent(in) :: expect_ext
        select type (v)
        type is (ext)
            if (.not. expect_ext) error stop 21
        type is (base)
            if (expect_ext) error stop 22
        class default
            error stop 23
        end select
    end subroutine
end module

! Fortran 2023 conditional arguments: the attributes, the type parameters and
! the shape of the argument are those of the consequent that is chosen
! (15.5.2.3), because that consequent is the actual argument. The consequents
! only have to agree in declared type, kind type parameters (C1538) and rank
! (C1539).
!
! This test is not labelled `gfortran`: GFortran 16 does not implement
! conditional arguments.
program conditional_arg_03
    use conditional_arg_03_mod
    implicit none
    integer, allocatable :: p(:), q(:)
    integer :: u(2), v(3)
    character(len=4) :: c4
    character(len=5) :: c5
    class(base), allocatable :: b, e

    ! An allocatable dummy argument keeps the allocatable consequent
    ! allocatable, and its bounds are those of the chosen one
    allocate(p(2)); p = 1
    allocate(q(3)); q = 2
    call want_allocatable( ( allocated(p) ? p : q ) )
    call grow( ( size(p)==2 ? p : q ) )
    if (size(p) /= 7) error stop 1
    if (size(q) /= 3) error stop 2

    ! Array consequents of the same rank and different shape
    u = [1,2]
    v = [3,4,5]
    if (total( ( .true.  ? u : v ) ) /= 3) error stop 3
    if (total( ( .false. ? u : v ) ) /= 12) error stop 4

    ! The length type parameter comes from the chosen consequent
    c4 = "abcd"
    c5 = "bcdef"
    if (my_len( ( .true.  ? c4 : c5 ) ) /= 4) error stop 5
    if (my_len( ( .false. ? c4 : c5 ) ) /= 5) error stop 6

    ! The declared type of both consequents is class(base), the dynamic type
    ! is that of the chosen one
    allocate(base :: b); b%n = 1
    allocate(ext :: e); e%n = 2
    call check_dynamic( ( .true.  ? e : b ), .true. )
    call check_dynamic( ( .false. ? e : b ), .false. )
contains
    subroutine want_allocatable(x)
        integer, allocatable, intent(in) :: x(:)
        if (.not. allocated(x)) error stop 24
        if (size(x) /= 2) error stop 25
    end subroutine

    subroutine grow(x)
        integer, allocatable, intent(inout) :: x(:)
        deallocate(x)
        allocate(x(7))
        x = 0
    end subroutine

    integer function total(x)
        integer, intent(in) :: x(:)
        total = sum(x)
    end function

    integer function my_len(s)
        character(len=*), intent(in) :: s
        my_len = len(s)
    end function
end program
