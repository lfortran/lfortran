program class_11
    implicit none
    type t_1
        character(:), allocatable :: s
    end type t_1
    type t
        class(t_1), allocatable :: x
    end type t
    type(t) :: type_1
    allocate(type_1%x)
    allocate(character(4) :: type_1%x%s)
    type_1%x%s = "1234"
    if (type_1%x%s /= "1234") error stop
end program class_11
