module pdt_10_module
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

end module pdt_10_module

program pdt_10
    use pdt_10_module, only: dp, tensor_t
    implicit none
    type(tensor_t(dp)) :: inputs

    if (abs(inputs%values() - 42.0_dp) > 1.0e-12_dp) error stop
    print *, "ok"
end program pdt_10
