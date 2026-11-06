program asynchronous_03
    ! ASYNCHRONOUS module variables and dummies of a module compiled
    ! separately (asynchronous_03_module.f90): the flag must be read back
    ! from the module file. a3 gets it from the statement in this scope.
    use asynchronous_03_module, only: a1, a2, a3, fill
    implicit none
    asynchronous :: a3
    call fill(a1, 1.0)
    call fill(a2, 2.0)
    call fill(a3, 3.0)
    if (any(a1 /= 1)) error stop
    if (any(a2 /= 2)) error stop
    if (any(a3 /= 3)) error stop
    print *, sum(a1) + sum(a2) + sum(a3)
end program asynchronous_03
