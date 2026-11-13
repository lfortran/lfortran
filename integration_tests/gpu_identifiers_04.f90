! A component reference in a procedure called from a device kernel keeps
! naming the component when a module procedure has the component's name.
module gpu_identifiers_04_m
    implicit none
    type :: pair
        integer :: step
    end type
contains
    pure integer function step(a) result(r)
        integer, intent(in) :: a
        r = a + 1
    end function

    pure integer function evaluate(value) result(r)
        integer, intent(in) :: value
        type(pair) :: t
        t%step = value
        r = step(t%step)
    end function
end module

program gpu_identifiers_04
    use gpu_identifiers_04_m, only: evaluate
    implicit none
    integer :: i, a(4)
    do concurrent (i = 1:4)
        a(i) = evaluate(i)
    end do
    if (any(a /= [2, 3, 4, 5])) error stop
    print *, "ok"
end program
