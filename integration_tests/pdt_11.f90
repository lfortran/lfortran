program pdt_11
    use pdt_11_module, only: dp, tensor_t
    implicit none

    type(tensor_t(dp)) :: inputs_dp
    real(dp) :: value_dp

    value_dp = inputs_dp%values()
    if (abs(value_dp - 42.0_dp) > 1.0e-12_dp) error stop
end program pdt_11
