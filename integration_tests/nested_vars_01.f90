module cobylb_mod_nested_vars_01
contains
    subroutine cobylb(amat)
    real, intent(in) :: amat(:, :)
    call evaluate(calcfc_internal)
    contains
        subroutine evaluate(calcfc)
            implicit none
            interface
                subroutine calcfc()
                    implicit none
                end subroutine calcfc
            end interface
            call calcfc()
        end subroutine evaluate

        subroutine calcfc_internal()
            implicit none
            real :: out(5, 5)
            print *, matprod(amat)
            out = matprod(amat)
            if (any(abs(out - 833.340454) > 1e-8)) error stop
            end subroutine calcfc_internal
        end subroutine

        function matprod(y) result(z)
            implicit none
            real, intent(in) :: y(:, :)
            real :: z(size(y,1), size(y, 2))

            z = matmul(y, y)
        end function matprod
end module


program nested_vars_01
use cobylb_mod_nested_vars_01
real :: amat(5, 5)
amat = 12.91
call cobylb(amat)
end program
