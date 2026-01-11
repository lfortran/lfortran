! This file checks correct type-conversions in print statements
! inside select statements
program class_71
    class(*), allocatable :: x
    allocate(x, source=42)
    select type (x)
    type is (integer)
        print *, x
    class default
        print *, "Unknown type"
    end select
end program class_71