module module_array_05
    type t
        real, allocatable :: x(:)
    end type t
end module module_array_05

program array_05
    use module_array_05
    implicit none

    type(t) :: type_01
    allocate(type_01%x(1, 2))
end program array_05
