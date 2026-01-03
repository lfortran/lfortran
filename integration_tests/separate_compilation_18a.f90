module mod_separate_compilation_18
    implicit none
    interface
        module function f(a) result(r)
        integer, intent(in) :: a
        integer :: r
        end function
    end interface
end module