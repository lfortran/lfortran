module pdt_19_m
    implicit none

    type :: int_t(k)
        integer, kind :: k = 4
        integer(k) :: v(2) = 1
        integer(k) :: w(2) = [1, 2]
        integer(k) :: z(2, 2) = 3_8
    end type

    type :: real_t(k)
        integer, kind :: k = 4
        real(k) :: x(2) = 1.5
        real(k) :: y(3) = [1.0, 2.0, 3.0]
    end type
end module

program pdt_19
    use pdt_19_m
    implicit none

    type(int_t) :: i4
    type(int_t(8)) :: i8
    type(real_t) :: r4
    type(real_t(8)) :: r8

    if (kind(i4%v) /= 4 .or. kind(i8%v) /= 8) error stop
    if (kind(i4%w) /= 4 .or. kind(i8%w) /= 8) error stop
    if (kind(i4%z) /= 4 .or. kind(i8%z) /= 8) error stop
    if (any(i4%v /= 1) .or. any(i8%v /= 1_8)) error stop
    if (any(i4%w /= [1, 2]) .or. any(i8%w /= [1_8, 2_8])) error stop
    if (any(i4%z /= 3) .or. any(i8%z /= 3_8)) error stop

    if (kind(r4%x) /= 4 .or. kind(r8%x) /= 8) error stop
    if (kind(r4%y) /= 4 .or. kind(r8%y) /= 8) error stop
    if (any(r4%x /= 1.5) .or. any(r8%x /= 1.5_8)) error stop
    if (any(r4%y /= [1.0, 2.0, 3.0])) error stop
    if (any(r8%y /= [1.0_8, 2.0_8, 3.0_8])) error stop
end program
