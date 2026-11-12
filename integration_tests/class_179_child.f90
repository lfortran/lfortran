module class_179_child_m
    use class_179_base_m, only: base
    implicit none
    type :: inner
        integer :: mark = 42
        integer, allocatable :: values(:)
        integer, pointer :: ref => null()
    end type
    type, extends(base) :: child
        type(inner) :: part(2)
    end type
end module
