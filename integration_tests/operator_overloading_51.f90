module operator_overloading_51_mod
    implicit none
    type :: d_t
        integer :: y
    contains
        procedure :: d_add
        generic :: operator(+) => d_add
    end type
contains
    elemental type(d_t) function d_add(a, b)
        class(d_t), intent(in) :: a
        type(d_t), intent(in) :: b
        d_add%y = a%y + b%y
    end function
end module

program operator_overloading_51
    use operator_overloading_51_mod
    implicit none
    type(d_t) :: s, v(3), r(3)

    s%y = 2
    v(1)%y = 1
    v(2)%y = 2
    v(3)%y = 3
    r = v + s
    print *, r%y
    if (any(r%y /= [3, 4, 5])) error stop
    if (size(v(2:3) + s) /= 2) error stop
    r = s + v
    if (any(r%y /= [3, 4, 5])) error stop
end program
