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

    type :: logical_t(k)
        integer, kind :: k = 4
        logical(k) :: p(2) = .true.
        logical(k) :: q(2) = .false.
    end type

    type :: complex_t(k)
        integer, kind :: k = 4
        complex(k) :: c(2) = (1.0, 2.0)
        complex(k) :: d(2) = [(1.0, 2.0), (3.0, 4.0)]
    end type
end module

program pdt_19
    use pdt_19_m
    implicit none

    type(int_t) :: i4
    type(int_t(8)) :: i8
    type(real_t) :: r4
    type(real_t(8)) :: r8
    type(logical_t) :: l4
    type(logical_t(8)) :: l8
    type(complex_t) :: c4
    type(complex_t(8)) :: c8

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

    if (kind(l4%p) /= 4 .or. kind(l8%p) /= 8) error stop
    if (kind(l4%q) /= 4 .or. kind(l8%q) /= 8) error stop
    if (.not. all(l4%p) .or. .not. all(l8%p)) error stop
    if (any(l4%q) .or. any(l8%q)) error stop

    if (kind(c4%c) /= 4 .or. kind(c8%c) /= 8) error stop
    if (kind(c4%d) /= 4 .or. kind(c8%d) /= 8) error stop
    if (any(c4%c /= (1.0, 2.0))) error stop
    if (any(c8%c /= (1.0_8, 2.0_8))) error stop
    if (any(c4%d /= [(1.0, 2.0), (3.0, 4.0)])) error stop
    if (any(c8%d /= [(1.0_8, 2.0_8), (3.0_8, 4.0_8)])) error stop
end program
