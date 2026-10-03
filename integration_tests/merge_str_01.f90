program merge_str_01
    implicit none
    character(len=5) :: a, b, c
    logical :: mask4
    logical(1) :: mask1
    a = "hello"
    b = "world"
    mask4 = .true.
    c = merge(a, b, mask4)
    if (c /= "hello") error stop
    mask1 = .false.
    c = merge(a, b, mask1)
    if (c /= "world") error stop
    mask1 = .true.
    c = merge(a, b, mask1)
    if (c /= "hello") error stop
    print *, "ok"
end program
