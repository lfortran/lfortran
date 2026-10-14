module traits_dependent_result_02_m
    implicit none

    abstract interface :: IValues
        pure function values(self, n) result(r)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: n
            integer :: r(n)
        end function
        pure function shifted_values(self, offset, n, zinput) result(r)
            type(itself), intent(in), pass :: self
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
