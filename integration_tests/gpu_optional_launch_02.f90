program gpu_optional_launch_02
    use gpu_optional_launch_02_mod
    implicit none
    integer :: i, n, required(37)
    integer, target :: bias(37)
    n = size(required)
    bias = 10
    call apply(n, required)
    if (any(required /= [(i, i = 1, 37)])) error stop 3
    call apply(n, required, bias)
    if (bias(1) /= 31 .or. any(bias(2:) /= 11)) error stop 4
    call apply(n, required)
    call apply_target(n)
    call apply_target(n, bias)
    if (bias(1) /= 52 .or. any(bias(2:) /= 12)) error stop 5
end program
