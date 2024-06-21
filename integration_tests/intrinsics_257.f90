program intrinsics_257
    implicit none
        integer(4), parameter :: i1 = product([1, 2, 3])
        real(4), parameter :: i2 = product([1.0, 2.0, 3.0])
        complex(4), parameter :: i3 = product([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)])
        integer(8), parameter :: i4 = product([1, 2, 3], [.true., .true., .true.])
        real(8), parameter :: i5 = product([1.5_8, 22.9_8, 3.0_8], mask=[.true., .false., .true.])
        complex(8), parameter :: i6 = product([(1.5_8, 2.2_8), (22.9_8, 1.4_8), (3.0_8, 1.1_8)], mask=[.true., .false., .true.])
        integer(4), parameter :: i7 = product([11, 2, 5], 1, mask=[.true., .false., .true.])
        real(4), parameter :: i8 = product([1.0, 3.0, 55.9], mask=[.true., .false., .true.], dim = 1)
        complex(4), parameter :: i9 = product([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)], dim = 1, mask=[.true., .false., .true.])
    
        integer(4) :: ar1(4) = [1, 2, 7, 9]
        real(4) :: ar2(4) = [1.0, 3.1, 7.2, 9.0]
        complex(4) :: ar3(4) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]
        logical(4) :: mask(4) = [.true., .false., .true., .true.]
        integer(4) :: dim = 1
    
        print *, i1
        if (i1 /= 6) error stop
        print *, i2
        if (abs(i2 - 6.0) > 1e-6) error stop
        print *, i3
        if (abs(i3 - (-85.0000000, 20.0000000)) > 1e-6) error stop
        print *, i4
        if (i4 /= 6) error stop
        print *, i5
        if (abs(i5 - 4.5_8) > 1e-12) error stop
        print *, i6
        if (abs(i6 - (2.0799999999999996,8.2500000000000000)) > 1e-6) error stop
        print *, i7
        if (i7 /= 55) error stop
        print *, i8
        if (abs(i8 - 55.9000015) > 1e-6) error stop
        print *, i9
        if (abs(i9 - (-7.00000000,16.0000000)) > 1e-6) error stop
    
        print *, product(ar1)
        if (product(ar1) /= 126) error stop
        print *, product(ar2)
        if (abs(product(ar2) - 200.879974) >1e-6 ) error stop
        print *, product(ar3)
        ! if (abs(product(ar3) - (-755.000000,-540.000000)) > 1e-6) error stop ! wrong answer
        print *, product(ar1, mask=mask)
        if (product(ar1, mask=mask) /= 63) error stop
        print *, product(ar2, mask=mask)
        if (abs(product(ar2, mask=mask) - 64.7999954) > 1e-6) error stop
        print *, product(ar3, mask=mask)
        ! if (abs(product(ar3, mask=mask) - (-177.000000, 56.0000000)) > 1e-6) error stop
        print *, product(ar1, dim=dim)
        if (product(ar1, dim=dim) /= 126) error stop
        print *, product(ar2, dim=dim)
        if (abs(product(ar2, dim=dim) - 200.879974) > 1e-6) error stop
        print *, product(ar3, dim=dim)
        ! if (abs(product(ar3, dim=dim) - (-755.000000,-540.000000)) > 1e-6) error stop
        print *, product(ar1, mask=mask, dim=dim)
        if (product(ar1, mask=mask, dim=dim) /= 63) error stop
        print *, product(ar2, mask=mask, dim=dim)
        if (abs(product(ar2, mask=mask, dim=dim) - 64.7999954) > 1e-6) error stop
        print *, product(ar3, mask=mask, dim=dim)
        ! if (abs(product(ar3, mask=mask, dim=dim) - (-177.000000,56.0000000)) > 1e-6) error stop
    
    end program
    