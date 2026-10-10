! A host variable of a program type passed to a sibling contained procedure.
! nested_vars moves the type into its context module, so the dummy arguments
! of the contained procedures must refer to the moved type too (#13745).
program nested_vars_18
    implicit none
    type :: box
        integer :: v
    end type
    type(box) :: x
    x%v = 1
    call inner()
    if (x%v /= 9) error stop 1
    call add(x, 3)
    if (x%v /= 12) error stop 2
    x = f(x, box(4))
    if (x%v /= 16) error stop 3
    print *, x%v
contains
    function f(a, b) result(c)
        type(box), intent(in) :: a, b
        type(box) :: c
        c%v = a%v + b%v
    end function
    subroutine add(a, n)
        type(box), intent(inout) :: a
        integer, intent(in) :: n
        a%v = a%v + n
    end subroutine
    subroutine inner()
        x = f(x, box(5))
        if (x%v /= 6) error stop 4
        call add(x, 3)
        if (x%v /= 9) error stop 5
    end subroutine
end program
