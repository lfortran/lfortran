module math_separate_compilation_21
    implicit none

    interface
        module function func(n, base) result(res)
            integer, intent(in) :: n
            integer, intent(in) :: base
            real :: res(max(n,0))
        end function func
    end interface

end module