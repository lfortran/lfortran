program real128_derived_type_01
    ! derived types with default-initialized real(16) components
    implicit none
    type t
        real(16) :: v = 1.5_16
        real(16) :: w = 2.0d0
        real(8) :: d = 0.5d0
        integer :: n = 3
    end type
    type u
        type(t) :: inner
        real(16) :: arr(2) = [1.0_16, 2.0_16]
    end type
    type(t) :: a, b(2)
    type(u) :: c
    type(t), parameter :: pc = t(4.0_16, 5.0_16, 1.0d0, 7)

    if (a%v /= 1.5_16) error stop 1
    if (a%w /= 2.0_16) error stop 2
    if (a%d /= 0.5d0) error stop 3
    if (a%n /= 3) error stop 4
    b = a
    if (b(2)%v /= 1.5_16) error stop 5
    b(1)%v = b(1)%v * 2
    if (b(1)%v /= 3.0_16) error stop 6
    if (c%inner%v /= 1.5_16) error stop 7
    if (c%arr(2) /= 2.0_16) error stop 8
    if (pc%v /= 4.0_16 .or. pc%n /= 7) error stop 9
    a = t(0.25_16, 0.125_16, 0.0d0, 0)
    if (a%v + a%w /= 0.375_16) error stop 10
    print *, "ok"
end program
