program intrinsics_309
    implicit none
    integer :: k
    integer(1), parameter :: xi1(2) = pack([1_1, 2_1, 3_1], [.true., .false., .true.])
    integer(2), parameter :: xi2(3) = pack([1_2, 2_2, 3_2], [.true., .true., .true.])
    integer(4), parameter :: xi3(2) = pack([1, 2, 3], [.true., .false., .true.])
    integer(8), parameter :: xi4(3) = pack([1_8, 2_8, 3_8], [.true., .true., .true.])
    integer(4), parameter :: xi5(*) = pack([(k**2, k = 1,3)], [(.true., k = 1,2), .false.])
    real, parameter :: x3(2) = pack([1.0, 2.0, 3.0], [.true., .false., .true.])
    real(8), parameter :: x4(3) = pack([1.0_8, 2.0_8, 3.0_8], [.true., .true., .true.])
    complex, parameter :: x5(2) = pack([(1.0, 1.0), (2.0, 2.0), (3.0, 3.0)], [.true., .false., .true.])
    complex(8), parameter :: x6(3) = pack([(1.0_8, 1.0_8), (2.0_8, 2.0_8), (3.0_8, 3.0_8)], [.true., .true., .true.])
    logical, parameter :: x7(2) = pack([.true., .false., .true.], [.true., .false., .true.])

    print *, xi1
    if (any(xi1 /= [1, 3])) error stop
    print *, xi2
    if (any(xi2 /= [1, 2, 3])) error stop
    print *, xi3
    if (any(xi3 /= [1, 3])) error stop
    print *, xi4
    if (any(xi4 /= [1, 2, 3])) error stop
    print *, xi5
    if (any(xi5 /= [1, 4])) error stop
    print *, x3
    if (any(x3 - [1.0, 3.0] > 1e-6)) error stop
    print *, x4
    if (any(x4 - [1.0_8, 2.0_8, 3.0_8] > 1e-12)) error stop
    print *, x5
    if (any(x5 /= [(1.0, 1.0), (3.0, 3.0)])) error stop
    print *, x6
    if (any(x6 /= [(1.0_8, 1.0_8), (2.0_8, 2.0_8), (3.0_8, 3.0_8)])) error stop
    print *, x7
    if (any(x7 .neqv. [.true., .true.])) error stop

end program