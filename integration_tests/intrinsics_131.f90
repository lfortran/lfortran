program intrinsics_131
    character(len=10) :: string_var, string_var2, string_var3
    string_var = "str"
    string_var2 = "string"
    string_var3 = "character"

    print*, max("str", "character")
    if (max("str", "character") /= "str") error stop

    print*, max(string_var, string_var2)
    if (max(string_var, string_var2) /= string_var2) error stop

    print*, max(string_var3, string_var2)
    if (max(string_var3, string_var2) /= string_var2) error stop
    
end program
