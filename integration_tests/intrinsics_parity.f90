program intrinsics_parity
    logical, dimension(2, 3) :: x
    logical :: parity_result_dim1(3)
    logical :: parity_result_dim2(2)
    x = reshape([.TRUE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.], [2, 3])

    if (parity([.TRUE., .FALSE.]) .neqv. .TRUE.) error stop
    if (parity([.TRUE., .FALSE., .FALSE., .TRUE.]) .neqv. .FALSE.) error stop

    parity_result_dim1 = parity(x, dim=1)
    if (parity_result_dim1(1) .neqv. .TRUE.) error stop
    if (parity_result_dim1(2) .neqv. .TRUE.) error stop
    if (parity_result_dim1(3) .neqv. .FALSE.) error stop

    parity_result_dim2 = parity(x, dim=2)
    if (parity_result_dim2(1) .neqv. .TRUE.) error stop
    if (parity_result_dim2(2) .neqv. .TRUE.) error stop
    print *, parity_result_dim2
end program intrinsics_parity
