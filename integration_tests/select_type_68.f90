! TYPE IS guard with a parameterized derived type named like a keyword
program select_type_68
    implicit none
    type :: in(n)
        integer, kind :: n
        integer(n) :: v
    end type
    type(in(4)) :: x
    type(in(8)) :: y
    class(*), allocatable :: a

    x%v = 3
    allocate(a, source=x)
    select type (a)
    type is (in(8))
        error stop 1
    type is (in(4))
        if (a%v /= 3) error stop 2
    class default
        error stop 3
    end select
    deallocate(a)

    y%v = 4_8
    allocate(a, source=y)
    select type (a)
    type is (in(4))
        error stop 4
    type is (in(8))
        if (a%v /= 4_8) error stop 5
    class default
        error stop 6
    end select
    print *, "ok"
end program
