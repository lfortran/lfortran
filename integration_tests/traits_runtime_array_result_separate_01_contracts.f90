! The trait of traits_runtime_array_result_separate_01: its messages return
! allocatable arrays, one of them sized by an argument, an array whose extent
! is another dummy, and an array shaped like an array argument.
module traits_runtime_array_result_separate_01_contracts_m
    implicit none
    abstract interface :: ISamples
        function samples(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x(:)
        end function samples
        function window(self, n) result(x)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            integer :: x(n)
        end function window
        function series(self, n) result(a)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            real(8), allocatable :: a(:)
        end function series
        function scaled(self, v) result(x)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: v(:)
            real(8) :: x(size(v))
        end function scaled
    end interface
end module traits_runtime_array_result_separate_01_contracts_m
