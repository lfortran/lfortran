! Recovery from errors in external procedures. `defined_twice` must stay the
! last case: a program unit that is aborted as a whole hides the errors in the
! bodies of the units after it, see
! https://github.com/lfortran/lfortran/issues/13758

subroutine rejected_postponed_bound(n)
    implicit none
    interface gen
        pure integer function gen_spec(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: n
    real :: r
    integer :: a(gen(r, n))  ! {Error} no specific procedure of `gen` matches
    a = 1
end subroutine

subroutine after_rejected_postponed_bound()
    implicit none
    integer :: k
    k = "abc"  ! {Error} still reported: the bound above is not left in scope
end subroutine

subroutine defined_twice(n)
    implicit none
    interface gen
        pure integer function gen_spec(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: n
    integer :: a(gen(n))
    a = 1
end subroutine

subroutine defined_twice(n)  ! {Error} already defined: its bound is dropped
    implicit none
    interface gen
        pure integer function gen_spec(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: n
    integer :: a(gen(n))
    a = 1
end subroutine

subroutine after_defined_twice()
end subroutine
