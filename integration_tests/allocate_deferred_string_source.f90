program allocate_deferred_string_source
    implicit none

    character(len=:), allocatable :: a(:)

    a = sg()

    if (size(a) /= 2) error stop "size"
    if (len(a) /= 5) error stop "len"
    if (a(1) /= "hello") error stop "val1"
    if (a(2) /= "world") error stop "val2"

contains

    function sg() result(res)
        character(len=:), allocatable :: res(:)
        character(len=5), save :: unnamed(2) = ["hello", "world"]

        allocate(res, source=unnamed)
    end function sg

end program allocate_deferred_string_source
