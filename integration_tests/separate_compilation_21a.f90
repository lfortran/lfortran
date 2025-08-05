module math_separate_compilation_21
    implicit none

    interface logspace
        module function func1(n, base) result(res)
            integer, intent(in) :: n
            integer, intent(in) :: base
            real :: res(max(n,0))
        end function func1

        module function func2(n, base) result(res)
            integer, intent(in) :: n
            real, intent(in) :: base
            integer :: res(max(n, 0))
        end function func2
    end interface

end module