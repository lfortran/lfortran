module submodule_60_m
    implicit none
    integer, target :: marr(3) = [7, 8, 9]
    interface
        module function s_check(filled) result(code)
            logical, intent(in) :: filled
            integer :: code
        end function
        module subroutine s_fill()
        end subroutine
        module function s2_check() result(code)
            integer :: code
        end function
    end interface
end module
