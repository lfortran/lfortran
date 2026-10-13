module traits_dependent_result_01_m
    implicit none

    abstract interface :: IText
        pure function text(n, input) result(r)
            integer, intent(in) :: n
            character(*), intent(in) :: input
            character(len=n) :: r
        end function
        pure function copy_text(input, n) result(r)
            character(*), intent(in) :: input
            integer, intent(in) :: n
            character(len=n) :: r
        end function
    end interface

contains
    function unused{IText :: T}(x) result(r)
        type(T), intent(in) :: x
        integer :: r
        r = 0
    end function
end module

program traits_dependent_result_01
    use traits_dependent_result_01_m
    implicit none
end program
