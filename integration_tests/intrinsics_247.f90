program intrinsics_247
    implicit none 
    character(len = 11), parameter :: space_at_end_ = adjustr('gfortran   ')
    character(len = 16), parameter :: space_in_between_ = adjustr('   g for tran   ')
    character(len = 25), parameter :: spaces_with_symbols_ = adjustr('  # gfor* t $ ran &    ')
    character(len = 2), parameter :: a(4) = adjustr(["ab", "cd", "ef", "gh"])
    character(len = 7), parameter :: b(3) = adjustr(["  hello", "  world", "fortran"])
    character(len = 10) :: c1
    character(len = 10) :: c2
    character(len = 10) :: c3(3)
    character(len = 10) :: res(3)

    c1 = "lfortran  "
    c2 = "  gfortran"
    c3 = [" fort ran ", "  lfortran", "gfortran  "]

    print *, adjustr(c1)
    if (adjustr(c1) /= "  lfortran") error stop
    print *, adjustr(c2)
    if (adjustr(c2) /= "  gfortran") error stop

    print *, space_at_end_
    if (space_at_end_ /= "   gfortran") error stop
    print *, space_in_between_
    if (space_in_between_ /= "      g for tran") error stop
    print *, spaces_with_symbols_
    if (spaces_with_symbols_ /= "      # gfor* t $ ran &") error stop

    print *, a
    if (any(a /= ["ab", "cd", "ef", "gh"])) error stop
    print *, b
    if (any(b /= ["  hello", "  world", "fortran"])) error stop
    print *, adjustr(["|a ", "b2 ", "3 c", " d4", "e| "])
    if (any(adjustr(["|a ", "b2 ", "3 c", " d4", "e| "]) /= [" |a", " b2", "3 c", " d4", " e|"])) error stop
    print *, adjustr(["lfortran  ", " compiler ", "opensource"])
    if (any(adjustr(["lfortran  ", " compiler ", "opensource"]) /= ["  lfortran", "  compiler", "opensource"])) error stop

    print *, adjustr([c1, c2])
    if (any(adjustr([c1, c2]) /= ["  lfortran", "  gfortran"])) error stop

    res = adjustr([c1, c2, c3(1)])
    print *, res
    if (any(res /= ["  lfortran", "  gfortran", "  fort ran"])) error stop

end program