program elemental_shifta_01
    implicit none
    integer :: scalar_val
    integer :: arr_shift(3)
    integer(8) :: wide_scalar
    
    scalar_val = 64
    arr_shift = [4, 2, 1]
    
    if (any(shifta(scalar_val, arr_shift) /= [4, 16, 32])) error stop
    
    if (any(shifta([64, 32, 16], 3) /= [8, 4, 2])) error stop
    
    wide_scalar = 1024_8
    if (any(shifta(wide_scalar, [1, 2]) /= [512_8, 256_8])) error stop
end program elemental_shifta_01
