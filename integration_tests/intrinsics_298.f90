program intrinsics_298
    implicit none

    integer, parameter :: x1 = dot_product([1, 2, 3], [4, 5, 6])
    integer(8), parameter :: x2 = dot_product([12_8, 33_8, 41_8], [13_8, 1_8, 7_8])
    real, parameter :: x3 = dot_product([3.0, 5.0, 11.0], [7.0, 13.0, 17.0])
    real(8), parameter :: x4 = dot_product([3.0_8, 5.0_8, 11.0_8], [7.0_8, 13.0_8, 17.0_8])
    complex, parameter :: x5 = dot_product([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)], [(7.0, 8.0), (9.0, 10.0), (11.0, 12.0)])
    complex(8), parameter :: x6 = dot_product([(1.0_8, 2.0_8), (3.0_8, 4.0_8), (5.0_8, 6.0_8)],&
        [(7.0_8, 8.0_8), (9.0_8, 10.0_8), (11.0_8, 12.0_8)])
    logical, parameter :: x7 = dot_product([.true., .false., .true.], [.true., .true., .false.])

    integer :: i1(3) = [13, 2, 4], i2(3) = [13_8, 2_8, 4_8]
    integer(8) :: i3(3) = [13_8, 2_8, 4_8], i4(3) = [13_8, 2_8, 4_8]
    real :: r1(3) = [13.0, 2.0, 4.0], r2(3) = [13.0, 2.0, 4.0]
    real(8) :: r3(3) = [13.0_8, 2.0_8, 4.0_8], r4(3) = [13.0_8, 2.0_8, 4.0_8]
    complex :: c1(3) = [(13.0, 2.0), (2.0, 4.0), (4.0, 6.0)], c2(3) = [(13.0, 2.0), (2.0, 4.0), (4.0, 6.0)]
    complex(8) :: c3(3) = [(13.0_8, 2.0_8), (2.0_8, 4.0_8), (4.0_8, 6.0_8)],& 
        c4(3) = [(13.0_8, 2.0_8), (2.0_8, 4.0_8), (4.0_8, 6.0_8)]
    logical :: l1(3) = [.true., .false., .true.], l2(3) = [.true., .false., .true.]

    print *, x1
    if (x1 /= 32) error stop
    print *, x2
    if (x2 /= 476) error stop
    print *, x3
    if (abs(x3 - 273.0) > 1e-6) error stop
    print *, x4
    if (abs(x4 - 273.0_8) > 1e-12) error stop
    print *, abs(x5)
    if (abs(abs(x5) - 217.745270) > 1e-5) error stop
    print *, abs(x6)
    if (abs(abs(x6) - 217.74526401279087_8) > 1e-12) error stop
    print *, x7
    if (.not. x7) error stop

    print *, dot_product(i1, i2)
    if (dot_product(i1, i2) /= 189) error stop
    print *, dot_product(i3, i4)
    if (dot_product(i3, i4) /= 189_8) error stop
    print *, dot_product(r1, r2)
    if (abs(dot_product(r1, r2) - 189.0) > 1e-6) error stop
    print *, dot_product(r3, r4)
    if (abs(dot_product(r3, r4) - 189.0_8) > 1e-12) error stop
    print *, abs(dot_product(c1, c2))
    if (abs(abs(dot_product(c1, c2)) - 245.0) > 1e-6) error stop
    print *, abs(dot_product(c3, c4))
    if (abs(abs(dot_product(c3, c4)) - 245.0_8) > 1e-12) error stop
    print *, dot_product(l1, l2)
    if (.not. dot_product(l1, l2)) error stop

    ! Cases with mixture of real, integer and complex
    print *, dot_product(i1, r2)
    if (abs(dot_product(i1, r2) - 189.0) > 1e-6) error stop
    print *, dot_product(r1, i2)
    if (abs(dot_product(i1, r2) - 189.0) > 1e-6) error stop
    print *, dot_product(c1, i2)
    if (abs(abs(dot_product(c1, i2)) - 197.699265) > 1e-6) error stop
    print *, dot_product(i1, c2) 
    if (abs(abs(dot_product(i1, c2)) - 197.699265) > 1e-6) error stop
    print *, dot_product(c4, r3)
    if (abs(abs(dot_product(c4, r3)) - 197.6992665641428_8) > 1e-12) error stop
    print *, dot_product(r4, c3)
    if (abs(abs(dot_product(r4, c3)) - 197.6992665641428_8) > 1e-12) error stop

end program