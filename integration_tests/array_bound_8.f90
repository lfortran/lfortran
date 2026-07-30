program array_bound_8
    ! An element of a PARAMETER (named-constant) array is a constant
    ! expression (F2018 10.1.12) and may be used as an explicit-shape
    ! array bound, a derived-type component bound, or a character length.

    implicit none

    integer, parameter :: dims(2) = [3, 4]
    integer, parameter :: n(1) = [5]

    ! 1-D array sized by a parameter-array element.
    real :: a(dims(1))
    ! multi-dimensional, mixing both elements of the parameter array.
    real :: b(dims(1), dims(2))
    ! the second element of the parameter array.
    real :: c(dims(2))

    ! a parameter-array element used as a derived-type component bound.
    type :: box
        real :: x(dims(1))
    end type box
    type(box) :: z

    ! a parameter-array element used as a character length.
    character(len=n(1)) :: s

    ! --- checks (all declarations above this point) ---

    if (size(a) /= 3) error stop
    if (size(b) /= 12) error stop
    if (size(c) /= 4) error stop
    if (size(z%x) /= 3) error stop
    if (len(s) /= 5) error stop

    print *, "pass"

end program
