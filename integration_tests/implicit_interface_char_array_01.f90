program implicit_interface_char_array_01
    character(48) :: namepol(5)
    namepol(1) = "foo"
    namepol(2) = "bar"
    call polplt(namepol)
end program implicit_interface_char_array_01
