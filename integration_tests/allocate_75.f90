program allocate_75
    ! A typed allocation applies its type-spec to every allocation object
    implicit none
    character(:), allocatable :: a, b, c(:), d(:)
    class(*), allocatable :: x, y
    integer :: n

    allocate(character(len=2) :: a, b)
    if (len(a) /= 2) error stop
    if (len(b) /= 2) error stop
    a = "ab"
    b = "cd"
    if (a // b /= "abcd") error stop

    n = 5
    allocate(character(n) :: c(3), d(4))
    if (len(c) /= 5) error stop
    if (len(d) /= 5) error stop
    if (size(c) /= 3) error stop
    if (size(d) /= 4) error stop
    c = "hello"
    d = "world"
    if (c(3) // d(4) /= "helloworld") error stop

    deallocate(a, b, c)
    allocate(character(len=n+1) :: a, c(2), b)
    if (len(a) /= 6) error stop
    if (len(b) /= 6) error stop
    if (len(c) /= 6) error stop
    if (size(c) /= 2) error stop

    allocate(integer :: x, y)
    select type (y)
    type is (integer)
        y = 42
    class default
        error stop
    end select
    select type (x)
    type is (integer)
    class default
        error stop
    end select
    select type (y)
    type is (integer)
        if (y /= 42) error stop
    class default
        error stop
    end select
    print *, "ok"
end program
