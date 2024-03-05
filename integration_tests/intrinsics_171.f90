program intrinsics_171
    
    character(5):: name = "hELLo"
    print*, _lfortran_tolowercase("HeLo#")
    if (_lfortran_tolowercase(name) /= "hello") error stop
    print*, _lfortran_tolowercase(name)
    if (_lfortran_tolowercase("HeLo#") /= "helo#") error stop

end