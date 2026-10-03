program associate_32
! Test: associate with deferred-length string concatenation expression
implicit none

associate(result => get_str() // "bar")
    if (result /= "foobar") error stop
end associate

associate(result => "hello" // " " // "world")
    if (result /= "hello world") error stop
end associate

associate(result => get_str())
    if (result /= "foo") error stop
end associate

contains
    pure function get_str() result(s)
        character(len=:), allocatable :: s
        s = "foo"
    end function
end program associate_32
