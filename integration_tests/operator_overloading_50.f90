module operator_overloading_50_a
    implicit none
    type :: date_type
        integer :: year
    end type
    interface operator(==)
        module procedure same_date
    end interface
contains
    elemental logical function same_date(a, b)
        type(date_type), intent(in) :: a, b
        same_date = a%year == b%year
    end function
end module

module operator_overloading_50_b
    implicit none
    type :: b_t
        integer :: x
    contains
        procedure :: b_lt
        generic :: operator(<) => b_lt
    end type
contains
    elemental logical function b_lt(a, b)
        class(b_t), intent(in) :: a
        type(b_t), intent(in) :: b
        b_lt = a%x < b%x
    end function
end module

program operator_overloading_50
    use operator_overloading_50_a
    use operator_overloading_50_b
    implicit none
    type(date_type) :: date, dates(3)
    type(b_t) :: s, v(3)
    logical :: l(3)

    date%year = 2024
    dates(1)%year = 2020
    dates(2)%year = 2024
    dates(3)%year = 2021
    print *, any(date == dates)
    if (.not. any(date == dates)) error stop
    if (count(date == dates) /= 1) error stop
    if (size(date == dates(2:3)) /= 2) error stop
    l = date == dates
    if (l(1) .or. .not. l(2) .or. l(3)) error stop

    s%x = 2
    v(1)%x = 1
    v(2)%x = 2
    v(3)%x = 3
    print *, v < s
    if (count(v < s) /= 1) error stop
    if (size(v(1:2) < s) /= 2) error stop
    l = v < s
    if (.not. l(1) .or. l(2) .or. l(3)) error stop
    l = s < v
    if (l(1) .or. l(2) .or. .not. l(3)) error stop
end program
