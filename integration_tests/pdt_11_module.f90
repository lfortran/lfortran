module pdt_11_module
    implicit none

    integer, parameter :: dp = kind(0.0d0)

    type tensor_t(k)
        integer, kind :: k = dp
    contains
        procedure :: values
    end type tensor_t

contains

    pure real(dp) function values(self)
        class(tensor_t(dp)), intent(in) :: self
        values = 42.0_dp
    end function values
end module pdt_11_module
