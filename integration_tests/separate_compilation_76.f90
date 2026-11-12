! A real(16) array with an initializer in a module procedure compiled to its
! own object file keeps its values (they were emitted as zeros)
program separate_compilation_76
    use separate_compilation_76a, only: sum_saved
    implicit none
    real(16) :: res
    call sum_saved(res)
    print *, res
    if (res /= 11.0_16) error stop
end program
