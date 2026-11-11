program derived_types_258
    use derived_types_258_m2
    implicit none
    integer :: c(2)
    c = codes()
    if (c(1) /= 2) error stop 1
    if (c(2) /= 4) error stop 2
end program
