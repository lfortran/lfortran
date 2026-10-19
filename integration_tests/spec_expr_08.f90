module spec_expr_08_m
    ! Specification expressions that call module functions defined later.
    implicit none
contains
    subroutine fill(b, x)
        integer, intent(inout) :: b(twice(1))
        integer, intent(in) :: x
        b = x
    end subroutine

    ! The result bound of `ones` calls `triple`, also defined later.
    pure integer function total(n)
        integer, intent(in) :: n
        integer :: r(size(ones(n)))
        r = 1
        total = sum(r) + inner(n)
    contains
        pure integer function inner(k)
            integer, intent(in) :: k
            integer :: w(plus1(k))
            w = 1
            inner = sum(w)
        end function
    end function

    ! Its locals name `total`, so they are built after `total`.
    pure function ones(n) result(r)
        integer, intent(in) :: n
        integer :: r(triple(n))
        type :: t
            procedure(total), pointer, nopass :: p => total
        end type
        type(t) :: y
        r = 1
        if (.not. associated(y%p, total)) r = 0
    end function

    pure integer function twice(n)
        integer, intent(in) :: n
        twice = 2*n
    end function

    pure integer function triple(n)
        integer, intent(in) :: n
        triple = 3*n
    end function

    pure integer function plus1(n)
        integer, intent(in) :: n
        plus1 = n + 1
    end function
end module

program spec_expr_08
    use spec_expr_08_m
    implicit none
    integer :: b(2)
    call fill(b, 4)
    if (any(b /= 4)) error stop
    print *, total(3)
    if (total(3) /= 13) error stop
end program
