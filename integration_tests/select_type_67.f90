! TYPE IS guards naming derived types that share their name with a keyword
module select_type_67_m
    implicit none
    type :: out
        integer :: k
    end type
    type, extends(out) :: inout
        integer :: j
    end type
    type :: none
        real :: r
    end type
    type :: value
        integer :: v
    end type
    type :: data
        integer :: d
    end type
    type :: result
        integer :: z
    end type
contains
    integer function classify(a) result(r)
        class(*), intent(in) :: a
        select type (a)
        type is (out)
            r = a%k
        type is (inout)
            r = 100 + a%j
        type is (none)
            r = 200 + int(a%r)
        type is (value)
            r = 300 + a%v
        type is (data)
            r = 400 + a%d
        type is (result)
            r = 500 + a%z
        type is (integer)
            r = 600 + a
        type is (integer(8))
            r = 700 + int(a)
        type is (real(4))
            r = 800 + int(a)
        type is (double precision)
            r = 900 + int(a)
        type is (character(*))
            r = 1000
        type is (logical)
            r = 1100
        class default
            r = -1
        end select
    end function
end module

program select_type_67
    use select_type_67_m
    implicit none
    class(*), allocatable :: a
    allocate(a, source=out(1))
    select type (a)
    type is (out)
        if (a%k /= 1) error stop 1
    class default
        error stop 2
    end select

    if (classify(out(3)) /= 3) error stop 3
    if (classify(inout(1, 4)) /= 104) error stop 4
    if (classify(none(5.0)) /= 205) error stop 5
    if (classify(value(6)) /= 306) error stop 6
    if (classify(data(7)) /= 407) error stop 7
    if (classify(result(8)) /= 508) error stop 8
    if (classify(9) /= 609) error stop 9
    if (classify(10_8) /= 710) error stop 10
    if (classify(11.0) /= 811) error stop 11
    if (classify(12.0d0) /= 912) error stop 12
    if (classify("abc") /= 1000) error stop 13
    if (classify(.true.) /= 1100) error stop 14
    if (classify((1.0, 2.0)) /= -1) error stop 15
    print *, "ok"
end program
