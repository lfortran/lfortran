! Operators and assignment. Type-bound operators, type-bound assignment
! and type-bound generics travel with the type, so they work when the type
! is accessed through a namespace. Non-type-bound defined operators have no
! name that could be qualified; they are imported with an ordinary
! USE ..., ONLY: operator(...) statement alongside the namespace.
module namespace_modules_14_vec
    implicit none
    type :: vec_t
        real :: x = 0, y = 0
    contains
        procedure :: add
        procedure :: assign_real
        generic :: operator(+) => add
        generic :: assignment(=) => assign_real
    end type

    interface operator(.dot.)
        module procedure dot
    end interface
contains
    type(vec_t) function add(a, b)
        class(vec_t), intent(in) :: a, b
        add%x = a%x + b%x
        add%y = a%y + b%y
    end function

    subroutine assign_real(a, r)
        class(vec_t), intent(inout) :: a
        real, intent(in) :: r
        a%x = r
        a%y = r
    end subroutine

    real function dot(a, b)
        type(vec_t), intent(in) :: a, b
        dot = a%x*b%x + a%y*b%y
    end function
end module

program namespace_modules_14
    use, namespace :: v => namespace_modules_14_vec
    use namespace_modules_14_vec, only: operator(.dot.)
    implicit none
    type(v%vec_t) :: a, b, c

    a = v%vec_t(1.0, 2.0)
    b = 3.0               ! type-bound assignment(=)
    if (abs(b%x - 3.0) > 1e-6 .or. abs(b%y - 3.0) > 1e-6) error stop
    c = a + b             ! type-bound operator(+)
    if (abs(c%x - 4.0) > 1e-6 .or. abs(c%y - 5.0) > 1e-6) error stop
    c = a                 ! intrinsic assignment
    if (abs(c%y - 2.0) > 1e-6) error stop
    if (abs((a .dot. b) - 9.0) > 1e-6) error stop
    ! The specific procedures are accessible by name
    if (abs(v%dot(a, a) - 5.0) > 1e-6) error stop
    c = v%add(a, a)
    if (abs(c%x - 2.0) > 1e-6) error stop
    print *, c%x, c%y
end program
