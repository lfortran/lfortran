module conditional_expr_08_mod
    implicit none
    type :: flags
        logical :: f
        logical, pointer :: p => null()
    end type
contains
    subroutine check_dummy(f, expect)
        logical, intent(in) :: f
        character(*), intent(in) :: expect
        character(3) :: s
        s = ( f ? "yes" : "no" )
        if (s /= expect) error stop 1
    end subroutine
end module

! Fortran 2023 conditional expressions with character arms whose condition is
! a logical variable, dummy argument, array element, component, pointer or
! allocatable, rather than a constant or an operation.
!
! This test is not labelled `gfortran`, for the same reason as
! conditional_expr_01 to 07 (see integration_tests/CMakeLists.txt).
program conditional_expr_08
    use conditional_expr_08_mod
    implicit none
    logical :: f, a(2)
    logical, target :: t
    logical, pointer :: p
    logical, allocatable :: q
    type(flags) :: x
    character(3) :: s, u, v
    character(:), allocatable :: d
    integer :: i

    f = .true.
    s = ( f ? "yes" : "no" )
    if (s /= "yes") error stop 2
    f = .false.
    s = ( f ? "yes" : "no" )
    if (s /= "no") error stop 3

    ! Character variables as arms, into a deferred length result
    u = "abc"; v = "xyz"
    d = ( f ? u : v )
    if (d /= "xyz") error stop 4
    if (len(d) /= 3) error stop 5

    ! The conditional expression as an operand rather than an assigned value
    if (( f ? "yes" : "no" ) /= "no") error stop 6
    if (( .not. f ? "yes" : "no" ) /= "yes") error stop 7
    d = "<" // ( f ? u : v ) // ">"
    if (d /= "<xyz>") error stop 8

    ! A dummy argument
    call check_dummy(.true., "yes")
    call check_dummy(.false., "no")

    ! An array element
    a = [.false., .true.]
    s = ( a(2) ? "yes" : "no" )
    if (s /= "yes") error stop 9
    s = ( a(1) ? "yes" : "no" )
    if (s /= "no") error stop 10

    ! A component, and a pointer component
    t = .true.
    x%f = .false.
    x%p => t
    s = ( x%f ? "yes" : "no" )
    if (s /= "no") error stop 11
    s = ( x%p ? "yes" : "no" )
    if (s /= "yes") error stop 12
    i = ( x%p ? 1 : 2 )
    if (i /= 1) error stop 13

    ! A pointer and an allocatable
    p => t
    s = ( p ? "yes" : "no" )
    if (s /= "yes") error stop 14
    allocate(q)
    q = .false.
    s = ( q ? "yes" : "no" )
    if (s /= "no") error stop 15

    print *, ( f ? "yes" : "no" )
end program
