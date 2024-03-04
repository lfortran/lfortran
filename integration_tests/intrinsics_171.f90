program main
    character(5):: name = "hELLo"
    print*, _lfortran_tolowercase("HeLo#")
    print*, _lfortran_tolowercase(name)
end