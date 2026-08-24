subroutine polplt(name)
    character(48) :: name(5)
    if (name(1) /= "foo") error stop
    if (name(2) /= "bar") error stop
end subroutine polplt
