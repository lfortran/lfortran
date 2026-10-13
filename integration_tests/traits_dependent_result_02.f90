module traits_dependent_result_02_m
    implicit none

    abstract interface :: IValues
        pure function values(n) result(r)
            integer, intent(in) :: n
            integer :: r(n)
        end function
        pure function shifted_values(offset, n, zinput) result(r)
            integer, intent(in) :: offset, n, zinput(n)
            integer :: r(n)
        end function
    end interface

contains
    function unused{IValues :: T}(x) result(r)
        type(T), intent(in) :: x
        integer :: r
        r = 0
    end function
end module

program traits_dependent_result_02
    use traits_dependent_result_02_m
    implicit none
end program
