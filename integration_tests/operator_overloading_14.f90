module operator_overloading_14_mod
    implicit none
    type, abstract :: base_type
    end type base_type

    type, extends(base_type) :: my_type
        integer :: x
    contains
        procedure :: my_type_equal
        generic :: operator(==) => my_type_equal
    end type
contains
    logical function my_type_equal(a, b)
        class(my_type), intent(in) :: a
        class(base_type), intent(in) :: b
        my_type_equal = .false.
        select type(b)
            type is (my_type)
                my_type_equal = (b%x == a%x)
        end select
    end function my_type_equal
end module operator_overloading_14_mod

program operator_overloading_14
    use operator_overloading_14_mod
    implicit none
    type(my_type) :: a, b
    a%x = 5
    b%x = 5
    if (.not. a == b) error stop
    b%x = 6
    if (a == b) error stop
end program operator_overloading_14