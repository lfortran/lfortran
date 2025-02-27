! test to ensure that commad separated string is read properly
! into an array of values
program string_48
    implicit none
    character(60) :: val1
    character(60) :: val2
    character(60) :: val3
    character(60) :: val4
    integer, save, dimension(3) :: nprocs_int32 = [-1,-1,-1]
    integer(8), save, dimension(4) :: nprocs_int64 = [-1,-1,-1,-1]
    real, save, dimension(3) :: nprocs_real32 = [-1,-1,-1]
    real(8), save, dimension(5) :: nprocs_real64 = [-1,-1,-1,-1,-1]
    val1 = "133,134,135"
    read(val1, *) nprocs_int32
    print *, nprocs_int32
    if (any(nprocs_int32 /= [133, 134, 135])) error stop

    val2 = "133,134,135,136"
    read(val2, *) nprocs_int64
    print *, nprocs_int64
    if (any(nprocs_int64 /= [133, 134, 135, 136])) error stop

    val3 = "133.,0.,1."
    read(val3, *) nprocs_real32
    print *, nprocs_real32
    if (any(nprocs_real32 /= [133., 0., 1.])) error stop

    val4 = "133.,0.,1.,0.,5."
    read(val4, *) nprocs_real64
    print *, nprocs_real64
    if (any(nprocs_real64 /= [133._8, 0.0_8, 1.0_8, 0.0_8, 5.0_8])) error stop
end program string_48
