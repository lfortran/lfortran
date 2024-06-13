program intrinsics_246
    implicit none 
    character(len = 25), parameter :: space_at_end_ = adjustl('gfortran   ')
    character(len = 25), parameter :: space_in_between_ = adjustl('   g for tran   ')
    character(len = 25), parameter :: spaces_with_symbols_ = adjustl('  # gfor* t $ ran &    ')
    character(len = 2), parameter :: a(4) = adjustl(["ab", "cd", "ef", "gh"])
    character(len = 7), parameter :: b(3) = adjustl(["  hello", "  world", "fortran"])
    character(len = 10) :: c1
    character(len = 10) :: c2
    character(len = 10) :: c3(3)
    character(len = 10) :: res(3)

    c1 = "lfortran  "
    c2 = "  gfortran"
    c3 = [" fort ran ", "  lfortran", "gfortran  "]

    print *, adjustl(c1)
    if (adjustl(c1) /= "lfortran  ") error stop
    print *, adjustl(c2)
    if (adjustl(c2) /= "gfortran  ") error stop

    print *, space_at_end_
    if (space_at_end_ /= "gfortran") error stop
    print *, space_in_between_
    if (space_in_between_ /= "g for tran") error stop
    print *, spaces_with_symbols_
    if (spaces_with_symbols_ /= "# gfor* t $ ran &") error stop

    print *, a
    if (any(a /= ["ab", "cd", "ef", "gh"])) error stop
    print *, b
    if (any(b /= ["hello  ", "world  ", "fortran"])) error stop
    print *, adjustl([" |a", "b2 ", "3 c", " d4", " e|"])
    if (any(adjustl([" |a", "b2 ", "3 c", " d4", " e|"]) /= ["|a ", "b2 ", "3 c", "d4 ", "e| "])) error stop
    print *, adjustl(["  lfortran", " compiler ", "opensource"])
    if (any(adjustl(["  lfortran", " compiler ", "opensource"]) /= ["lfortran  ", "compiler  ", "opensource"])) error stop

    print *, adjustl([c1, c2])
    if (any(adjustl([c1, c2]) /= ["lfortran  ", "gfortran  "])) error stop

    res = adjustl([c1, c2, c3(1)])
    print *, res
    if (any(res /= ["lfortran  ", "gfortran  ", "fort ran  "])) error stop

end program
