program separate_compilation_26
    use quadrature_separate_compilation_26 , only: gauss_legendre
    implicit none
    
    integer :: x = 1
    call gauss_legendre(x)

    print *, x
    if (x /= 2) error stop
end program