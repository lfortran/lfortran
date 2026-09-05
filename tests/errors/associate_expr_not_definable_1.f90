! An associate name whose selector is an expression, and not a variable, must
! not appear in a variable definition context (F2018 11.1.3.3).
program associate_expr_not_definable_1
    implicit none
    integer :: a(3)
    integer :: i
    a = [1, 2, 3]
    associate (x => a + 1)
        x = 4  ! {Error} Associate name `x` is associated with an expression and cannot be used in a variable definition context (assignment)
        x(2) = 5  ! {Error} Associate name `x` is associated with an expression and cannot be used in a variable definition context (assignment)
        read(*, *) x  ! {Error} Associate name `x` is associated with an expression and cannot be used in a variable definition context (READ statement)
    end associate
    associate (n => size(a))
        do n = 1, 3  ! {Error} Associate name `n` is associated with an expression and cannot be used in a variable definition context (DO loop variable)
        end do
    end associate
    associate (y => f_val())
        y = 4  ! {Error} Associate name `y` is associated with an expression and cannot be used in a variable definition context (assignment)
    end associate
    ! Associating with a name that is not definable does not make it definable.
    associate (p => a + 1)
        associate (q => p)
            q = 4  ! {Error} Associate name `q` is associated with an expression and cannot be used in a variable definition context (assignment)
        end associate
    end associate
    ! An associate name whose selector is a variable stays definable, and the
    ! name is only restricted inside its own construct.
    associate (v => a)
        v = 4
        i = v(2)
    end associate
    print *, i
contains
    function f_val() result(w)
        integer :: w(3)
        w = [1, 2, 3]
    end function
end program
