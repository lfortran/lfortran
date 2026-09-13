module conditional_expr_07_mod
    implicit none
    type :: base
        integer :: n
    end type
    type, extends(base) :: ext
        integer :: m
    end type
contains
    subroutine check(x, expect_ext)
        class(base), intent(in) :: x
        logical, intent(in) :: expect_ext
        select type (x)
        type is (ext)
            if (.not. expect_ext) error stop 21
        type is (base)
            if (expect_ext) error stop 22
        class default
            error stop 23
        end select
    end subroutine
end module

! Fortran 2023 conditional expressions with derived type and polymorphic arms.
! The declared type and kind must agree (C1004), while the dynamic type comes
! from the arm that is chosen (10.1.4 p22-23).
!
! This test is not labelled `gfortran`: GFortran 16 rejects these with "Sorry,
! only integer, logical, real, complex and character types are currently
! supported for conditional expressions".
program conditional_expr_07
    use conditional_expr_07_mod
    implicit none
    type(base) :: b1, b2, bx
    class(base), allocatable :: p1, p2, px
    class(base), allocatable :: e1
    integer, target :: ta, tb
    integer :: x

    ! Both arms have the same declared derived type (7.5)
    b1 = base(1); b2 = base(2)
    bx = ( .true. ? b1 : b2 )
    if (bx%n /= 1) error stop 1
    bx = ( .false. ? b1 : b2 )
    if (bx%n /= 2) error stop 2

    ! A structure constructor as an arm (7.5.10)
    bx = ( .true. ? base(5) : base(6) )
    if (bx%n /= 5) error stop 3

    ! Polymorphic arms: the result is polymorphic (10.1.4 p23)
    allocate(base :: p1); p1%n = 1
    allocate(base :: p2); p2%n = 2
    px = ( .true. ? p1 : p2 )
    if (px%n /= 1) error stop 4
    px = ( .false. ? p1 : p2 )
    if (px%n /= 2) error stop 5

    ! The dynamic type comes from the arm that is chosen, while the declared
    ! type of both arms is class(base)
    allocate(ext :: e1)
    e1%n = 7
    call check( ( .true.  ? e1 : p1 ), .true. )
    call check( ( .false. ? e1 : p1 ), .false. )

    ! A conditional expression yields a value, so a pointer target used as an
    ! arm contributes its value and not its association
    ta = 1; tb = 2
    x = ( .true. ? ta : tb )
    if (x /= 1) error stop 6
end program
