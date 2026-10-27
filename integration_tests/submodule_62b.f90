module submodule_62_m1
    implicit none
    interface
        module subroutine s_step(r, q)
            integer, intent(out) :: r, q
        end subroutine
        module function s_class_ok() result(ok)
            logical :: ok
        end function
    end interface
end module

module submodule_62_m2
    implicit none
    interface
        module subroutine t_step(r, q)
            integer, intent(out) :: r, q
        end subroutine
        module function t_q_total() result(n)
            integer :: n
        end function
    end interface
end module
