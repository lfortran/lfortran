program allocate_38
    implicit none

    character(len=:), allocatable :: a(:)
    a = sg()
    if (a(1) /= "hello" .or. a(2) /= "world") error stop "sg() returned wrong values"

contains

    function sg() result(res)
        character(len=:), allocatable :: res(:)
        character(len=5), parameter :: unnamed(2) = ["hello", "world"]
        allocate(res, source=unnamed)
    end function sg

end program allocate_38
