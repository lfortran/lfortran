program realloc_lhs_24
    implicit none
    character(len=:), allocatable :: list(:), joined(:)
    character(len=80), allocatable :: fixed(:)

    list = [character(len=0) ::]
    if (size(list) /= 0) error stop
    if (len(list) /= 0) error stop

    fixed = [character(len=80) :: "hello", "world"]
    if (size(fixed) /= 2) error stop
    if (len(fixed) /= 80) error stop
    if (trim(fixed(1)) /= "hello") error stop
    if (trim(fixed(2)) /= "world") error stop

    list = [character(len=4) :: "help"]
    if (size(list) /= 1) error stop
    if (len(list) /= 4) error stop
    if (list(1) /= "help") error stop

    joined = [character(len=4) :: "tool", list]
    list = joined
    if (size(list) /= 2) error stop
    if (len(list) /= 4) error stop
    if (list(1) /= "tool") error stop
    if (list(2) /= "help") error stop

    print *, "PASS"
end program
