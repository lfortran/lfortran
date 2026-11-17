program real10_out_of_range_scalar_fold_01
    ! out_of_range of real(10) scalar constants is decided exactly, not
    ! through double: 2**63 - 0.5 truncates to huge(1_8) and rounds to 2**63;
    ! -2**63 - 0.75 is -2**63 - 1 in real(10)
    implicit none
    real(10), parameter :: o1 = 2.0_10**63 - 0.5_10, o2 = -2.0_10**63 - 0.75_10
    real(10), parameter :: o3 = 2.0_10**63, o4 = -2.0_10**63
    real(10), parameter :: o5 = 2.0_10**31 - 0.25_10
    logical, parameter :: t1 = out_of_range(o1, 1_8), t2 = out_of_range(o2, 1_8)
    logical, parameter :: t3 = out_of_range(o3, 1_8), t4 = out_of_range(o4, 1_8)
    logical, parameter :: r1 = out_of_range(o1, 1_8, .true.)
    logical, parameter :: r4 = out_of_range(o4, 1_8, .true.)
    logical, parameter :: k4 = out_of_range(o5, 1), k4r = out_of_range(o5, 1, .true.)
    logical, parameter :: k2 = out_of_range(32767.5_10, 1_2)
    logical, parameter :: k2r = out_of_range(32767.5_10, 1_2, .true.)
    logical, parameter :: f4 = out_of_range(1.0e38_10, 1.0_4)
    logical, parameter :: f4b = out_of_range(4.0e38_10, 1.0_4)
    logical, parameter :: f8 = out_of_range(-1.0e309_10, 1.0_8)
    logical, parameter :: f10 = out_of_range(-4.0e4000_10, 1.0_10)

    if (t1 .or. .not. t2 .or. .not. t3 .or. t4) error stop "int8"
    if (.not. r1 .or. r4) error stop "int8 round"
    if (k4 .or. .not. k4r) error stop "int4"
    if (k2 .or. .not. k2r) error stop "int2"
    if (f4 .or. .not. f4b .or. .not. f8 .or. f10) error stop "real"
    print *, t1, t2, r1, k4r
end program
