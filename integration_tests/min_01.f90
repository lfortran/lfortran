program min_min
    real(8) :: real_inp1
    real(8) :: real_inp2
    real(4) :: real_inp3
    real(4) :: real_inp4
    integer(4) :: int_inp1
    integer(4) :: int_inp2
    
    real(8) :: output_min
    integer(4) :: output_min0
    real(4) :: output_amin0
    integer(4) :: output_min1
    real(4) :: output_amin1
    real(8) :: output_dmin1
    
    real_inp1 = 5.0d0
    real_inp2 = 10.0d0
    real_inp3 = 5.0
    real_inp4 = 10.0
    int_inp1 = 5
    int_inp2 = 10
     
    output_min = min(real_inp1, real_inp2)
    print*, output_min
    if (abs(output_min - 5) >= 1e-15) error stop

    output_min0 = min0(int_inp1, int_inp2)
    print*, output_min0
    if (output_min0 /= 5) error stop

    output_amin0 = amin0(int_inp1, int_inp2)
    print*, output_amin0
    if (abs(output_amin0 - 5) >= 1e-7) error stop

    output_min1 = min1(real_inp3, real_inp4)
    print*, output_min1
    if (output_min1 /= 5) error stop

    output_amin1 = amin1(real_inp3, real_inp4)
    print*, output_amin1
    if (abs(output_amin1 - 5) >= 1e-7) error stop

    output_dmin1 = dmin1(real_inp1, real_inp1)
    print*, output_dmin1
    if (abs(output_dmin1 - 5) >= 1e-15) error stop

 end program
 
 