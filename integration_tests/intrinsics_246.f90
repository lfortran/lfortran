program intrinsics_246
    implicit none 
    character(len = 2), parameter :: a(4) = adjustl(["ab", "cd", "ef", "gh"])
    character(len = 7), parameter :: b(3) = adjustl(["  hello", "  world", "fortran"])

    print*, a
    if (any(adjustl(["ab", "cd", "ef", "gh"]) /= ["ab", "cd", "ef", "gh"])) error stop

    print*, b
    if (any(adjustl(["  hello", "  world", "fortran"]) /= ["hello  ", "world  ", "fortran"])) error stop
  
    print*, adjustl([" |a", "b2 ", "3 c", " d4", " e|"])
    if (any(adjustl([" |a", "b2 ", "3 c", " d4", " e|"]) /= ["|a ", "b2 ", "3 c", "d4 ", "e| "])) error stop

    print*, adjustl(["  lfortran", " compiler ", "opensource"])
    if (any(adjustl(["  lfortran", " compiler ", "opensource"]) /= ["lfortran  ", "compiler  ", "opensource"])) error stop

end program
