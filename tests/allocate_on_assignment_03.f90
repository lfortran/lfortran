module allocate_on_assignment_03_m
    implicit none
    type :: t
        integer :: v = 0
    end type

    type, extends(t) :: t2
        integer :: w = 0
    end type
end module

program allocate_on_assignment_03
    use allocate_on_assignment_03_m
    implicit none
    class(t), allocatable :: a, b
    class(*), allocatable :: u, q
    allocate(t2 :: b)
    a = b
    a = a
    q = 1
    u = q
    print *, a%v
end program
