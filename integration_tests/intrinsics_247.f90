
program intrinsics_247
    implicit none 
    character(len = 2), parameter :: a(4) = adjustr(["ab", "cd", "ef", "gh"])
    character(len = 7), parameter :: b(3) = adjustr(["  hello", "  world", "fortran"])

    print*, a
    if (any(adjustr(["ab", "cd", "ef", "gh"]) /= ["ab", "cd", "ef", "gh"])) error stop

    print*, b
    if (any(adjustr(["  hello", "world  ", "fortran"]) /= ["  hello", "  world", "fortran"])) error stop
  
    print*, adjustr(["|a ", "b2 ", "3 c", " d4", "e| "])
    if (any(adjustr(["|a ", "b2 ", "3 c", " d4", "e| "]) /= [" |a", " b2", "3 c", " d4", " e|"])) error stop

    print*, adjustr(["lfortran  ", " compiler ", "opensource"])
    if (any(adjustr(["lfortran  ", " compiler ", "opensource"]) /= ["  lfortran", "  compiler", "opensource"])) error stop

end program