program intrinsics_92
    real(4) x4
    real(8) x8
    x4 = 1.234E0_4
    x8 = -4.721_8
    if ( abs(anint(x4) - 1.0) > 1e-5 ) error stop
    if ( abs(anint(x8) + 5.0) > 1e-5) error stop
    x8 = anint(x4,8)
    if ( abs(x8 - 1.0) > 1e-5 ) error stop
end
