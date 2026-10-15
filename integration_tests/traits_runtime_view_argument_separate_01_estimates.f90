! The inner trait of traits_runtime_view_argument_separate_01, in a module of
! its own, which the contracts module imports for its message arguments.
module traits_runtime_view_argument_separate_01_estimates_m
    implicit none
    abstract interface :: IEstimates
        subroutine some_method(self)
            type(itself), pass, intent(inout) :: self
        end subroutine some_method
        integer function total(self)
            type(itself), pass, intent(in) :: self
        end function total
    end interface
end module traits_runtime_view_argument_separate_01_estimates_m
