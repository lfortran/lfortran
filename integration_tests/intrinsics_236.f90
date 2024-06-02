program intrinsics_236
    implicit none
    integer,parameter:: ik1 = selected_int_kind(1)
    print "(A,I0)", &
        'ik1                   = ',ik1,&
        'kind(bit_size(1_ik1)) = ',kind(bit_size(1_ik1)), &
        'kind(huge(1_ik1))     = ',kind(huge(1_ik1)),&
        'huge(1_ik1)           = ',huge(1_ik1)

    if (1_ik1 /= 1) error stop
    if (kind(bit_size(1_ik1)) /= 1) error stop
    if (kind(huge(1_ik1)) /= 1) error stop
    if (huge(1_ik1) /= 127) error stop

end program
