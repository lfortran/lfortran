module module_1

    implicit none
    type t_1
        integer :: i
    end type t_1

    type, extends(t_1) :: t_2
        real :: r
    end type t_2

end module module_1

program derived_types_10
    use module_1
    implicit none

    type(t_1) :: type_1
    type(t_2) :: type_2

    type_1%i = 123
    type_2%t_1 = type_1
    type_2%r = 10.00

    if (type_2%i /= 123) error stop
    if (type_2%r /= 10.00) error stop
end program derived_types_10
