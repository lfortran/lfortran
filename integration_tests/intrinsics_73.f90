program intrinsics_73
    real(8) :: y
    real ::  x
    y = 5.2d0
    x = sngl(y)
    if(abs(x - 5.19999981)>1e-8) error stop
    ! if(kind(sngl(y)) /= 4) error stop //TODO
end program