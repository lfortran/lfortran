module allocate_on_assignment_02_m
    implicit none
    type :: t
        integer :: v = 0
    end type

    type, extends(t) :: t2
        integer :: w = 0
    end type
end module

program allocate_on_assignment_02
    use allocate_on_assignment_02_m
    implicit none
    class(t), allocatable :: a
    class(*), allocatable :: u
    a = t2(1, 2)
    u = 3
    print *, a%v
end program
