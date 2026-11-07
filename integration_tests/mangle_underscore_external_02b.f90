subroutine mue02_add(k, n)
    integer, intent(inout) :: k
    integer, intent(in) :: n
    call inner()
contains
    subroutine inner()
        k = k + n
    end subroutine
end subroutine

integer function mue02_twice(n)
    integer, intent(in) :: n
    mue02_twice = 2*n
end function

subroutine mue02_call_back(k)
    integer, intent(inout) :: k
    interface
        subroutine mue02_seven(k)
            integer, intent(inout) :: k
        end subroutine
    end interface
    call mue02_seven(k)
end subroutine
