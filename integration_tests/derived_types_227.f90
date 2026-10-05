! A function returning a derived type is assigned to a pointer or
! allocatable variable that is also its argument. The result goes through a
! temporary, which must be a pointer or allocatable only when the function
! result is.
program derived_types_227
    implicit none
    type :: box
        integer :: v
    end type
    type(box), target :: y
    type(box), pointer :: p
    type(box), allocatable :: q
    type(box) :: x

    y%v = 1
    p => y
    p = f(p)
    if (y%v /= 9) error stop 1
    if (.not. associated(p, y)) error stop 2

    allocate(q)
    q%v = 2
    q = f(q)
    if (q%v /= 10) error stop 3

    q = g(q)
    if (q%v /= 20) error stop 4

    p = g(p)
    if (y%v /= 19) error stop 5

    x%v = 3
    x = g(x)
    if (x%v /= 13) error stop 6

    print *, y%v, q%v, x%v
contains
    function f(a) result(c)
        type(box), intent(in) :: a
        type(box) :: c
        c%v = a%v + 8
    end function
    function g(a) result(c)
        type(box), intent(in) :: a
        type(box), allocatable :: c
        allocate(c)
        c%v = a%v + 10
    end function
end program
