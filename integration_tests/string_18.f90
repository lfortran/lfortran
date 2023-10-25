program string_18
    implicit none
    character(len=3) :: x
    character(len=5) :: y
    x = "123"
    y = "123  "

    ! Compile time
    if ("123 " < "123") error stop
    if ("123 " /= "123") error stop
    if ("123 " > "123") error stop
    if ("123	" > "123	") error stop

    ! Run time
    if (x /= y) error stop
    if (.not.(x == y)) error stop
    if (x > y) error stop
    if (x < y) error stop
    y = "  123"
    if (y /= "  123  ") error stop
    y = "    "
    if (y /= "") error stop
    y = "  1"
    if (y /= "  1") error stop
    y = "1	"
    if (y /= "1	") error stop

end program string_18
