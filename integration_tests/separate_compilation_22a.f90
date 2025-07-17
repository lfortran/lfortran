module mod_separate_compilation_22
    implicit none
    interface
        module function f_sc_22(a) result(r)
        integer, intent(in) :: a
        integer :: r
        end function
    end interface
end module