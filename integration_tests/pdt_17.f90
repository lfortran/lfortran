program pdt_17
    implicit none

    type empty_t(k)
        integer, kind :: k
    end type

    type value_t(k)
        integer, kind :: k
        real(k) :: value
    end type

    type default_t(k)
        integer, kind :: k = 4
        real(k) :: value
    end type

    type(empty_t(4)) :: empty4 = empty_t(4)()
    type(empty_t(8)) :: empty8 = empty_t(k=8)()
    type(value_t(4)) :: value4 = value_t(4)(1.5)
    type(value_t(8)) :: value8 = value_t(8)(2.25_8)
    type(value_t(8)) :: keyword8 = value_t(k=8)(3.5_8)
    type(default_t(8)) :: override8 = default_t(k=8)(5.25_8)
    type(default_t) :: default4 = default_t(value=6.5)
    type(value_t(8)), parameter :: constant8 = value_t(8)(1.23456789012345_8)

    if (empty4%k /= 4 .or. empty8%k /= 8) error stop
    if (value4%k /= 4 .or. kind(value4%value) /= 4) error stop
    if (value8%k /= 8 .or. kind(value8%value) /= 8) error stop
    if (value4%value /= 1.5) error stop
    if (value8%value /= 2.25_8) error stop
    if (keyword8%value /= 3.5_8) error stop
    if (override8%k /= 8 .or. kind(override8%value) /= 8) error stop
    if (override8%value /= 5.25_8) error stop
    if (default4%k /= 4 .or. default4%value /= 6.5) error stop
    if (abs(constant8%value - 1.23456789012345_8) > 1.e-14_8) error stop

    value4 = value_t(4)(8.5)
    value8 = value_t(k=8)(9.25_8)
    if (value4%value /= 8.5 .or. value8%value /= 9.25_8) error stop
end program
