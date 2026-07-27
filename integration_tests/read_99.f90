program read_99
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: v, w
    character(len=30) :: s

    s = "1.5"
    read(s, *) v
    if (abs(v - 1.5_real128) > 1.0e-30_real128) error stop 1

    s = "0.25, -2.5"
    read(s, *) v, w
    if (abs(v - 0.25_real128) > 1.0e-30_real128) error stop 2
    if (abs(w + 2.5_real128) > 1.0e-30_real128) error stop 3

    s = "1.0d-1"
    read(s, *) v
    if (abs(v - 0.1_real128) > 1.0e-30_real128) error stop 4

    print *, "ok"
end program read_99
