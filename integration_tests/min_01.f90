program min_min
    real(8) :: real_inp1
    real(8) :: real_inp2
    real(4) :: real_inp3
    real(4) :: real_inp4
    real(8) :: real_inp5
    real :: real_inp6
    integer(4) :: int_inp1
    integer(4) :: int_inp2
    
    real(8) :: output_min
    integer(4) :: output_min0
    real(4) :: output_amin0
    integer(4) :: output_min1
    real(4) :: output_amin1
    real(8) :: output_dmin1
    real(8) :: output_min_different_kinds
    
    real_inp1 = 5.0d0
    real_inp2 = 10.0d0
    real_inp3 = 5.0
    real_inp4 = 10.0
    int_inp1 = 5
    int_inp2 = 10
    real_inp5 = 5.2d0
    real_inp6 = 9.0
     
    output_min = min(real_inp1, real_inp2)
    print*, output_min
    if (abs(output_min - 5) >= 1e-15) error stop
    if( kind(output_min) /= 8) error stop "Incorrect kind for min"

    output_min0 = min0(int_inp1, int_inp2)
    print*, output_min0
    if (output_min0 /= 5) error stop
    if( kind(output_min0) /= 4) error stop "Incorrect kind for min"

    output_amin0 = amin0(int_inp1, int_inp2)
    print*, output_amin0
    if (abs(output_amin0 - 5) >= 1e-7) error stop
    if (kind (output_amin0) /= 4) error stop "Incorrect kind for min"

    output_min1 = min1(real_inp3, real_inp4)
    print*, output_min1
    if (output_min1 /= 5) error stop
    if( kind(output_min1) /= 4) error stop "Incorrect kind for min"

    output_amin1 = amin1(real_inp3, real_inp4)
    print*, output_amin1
    if (abs(output_amin1 - 5) >= 1e-7) error stop
    if( kind(output_amin1) /= 4) error stop "Incorrect kind for min"

    output_dmin1 = dmin1(real_inp1, real_inp1)
    print*, output_dmin1
    if (abs(output_dmin1 - 5) >= 1e-15) error stop
    if( kind(output_dmin1) /= 8) error stop "Incorrect kind for min"

    output_min_different_kinds = min(real_inp5, real_inp6)
    print *, output_min_different_kinds
    if( abs( output_min_different_kinds - 5.2d0) > 1e-16) error stop
    if ( Kind(output_min_different_kinds) /= 8) error stop "Incorrect kind for min"

 end program
 
 