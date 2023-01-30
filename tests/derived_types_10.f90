program name
    implicit none

    type t_1
        integer :: i
    end type t_1

    type, extends(t_1) :: t_2
        real :: r
    end type t_2

    type(t_1) :: type_1
    type(t_2) :: type_2

    type_2%i = 1
    type_2%t_1 = type_1

    print *, type_1%i, type_2%i, type_2%r
end program name
