program intrinsics_88
    character(len=10) :: string_var, string_var2, string_var3
    string_var = "str"
    string_var2 = "string"
    string_var3 = "character"

    print*, min("str", "character")
    if (min("str", "character") /= "character") error stop

    print*, min(string_var, string_var2)
    if (min(string_var, string_var2) /= string_var) error stop

    print*, min(string_var3, string_var2)
    if (min(string_var3, string_var2) /= string_var3) error stop
end program