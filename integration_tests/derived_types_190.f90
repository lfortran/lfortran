program derived_types_190
    implicit none

    type :: t
        character(len=:), allocatable :: a(:)
    end type t

    type(t) :: v

    allocate(character(len=3) :: v%a(2))
    v%a = ["mmm", "nnn"]
    if (len(v%a) /= 3) error stop "first assignment: wrong length"
    if (size(v%a) /= 2) error stop "first assignment: wrong size"
    if (v%a(1) /= "mmm") error stop "first assignment: first element"
    if (v%a(2) /= "nnn") error stop "first assignment: second element"

    deallocate(v%a)
    allocate(character(len=4) :: v%a(3))
    v%a = ["oooo", "pppp", "qqqq"]
    if (len(v%a) /= 4) error stop "second assignment: wrong length"
    if (size(v%a) /= 3) error stop "second assignment: wrong size"
    if (v%a(1) /= "oooo") error stop "second assignment: first element"
    if (v%a(3) /= "qqqq") error stop "second assignment: third element"

    print *, "ok"

end program derived_types_190
