module derived_types_69_m
    type, abstract :: AbsType
    end type AbsType

    integer, parameter :: a(2) = [5, 6]
    integer, parameter :: one = 1

    type, extends(AbsType) :: MyType
        integer :: m1(2) = [one, one]
        integer :: m2(2) = a
        integer :: m3(2) = [integer :: 2, 3]
    end type MyType
end module derived_types_69_m

program derived_types_69
    use derived_types_69_m

    class(MyType), allocatable :: obj

    obj = MyType()

    if (any(obj%m1 /= [1, 1])) error stop
    if (any(obj%m2 /= [5, 6])) error stop
    if (any(obj%m3 /= [2, 3])) error stop
end program derived_types_69
