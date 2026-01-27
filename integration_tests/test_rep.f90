module rep_test_mod
    implicit none
contains
    subroutine f()
        character(len=:), allocatable :: x, y, z
        x = "Hello"
        z = "Aa1"
        y = _lfortran_rep(x, 3)
        if ( y /= "HelloHelloHello" ) error stop

        y = _lfortran_rep(z, 5)
        if ( y /= "Aa1Aa1Aa1Aa1Aa1" ) error stop
        
        ! TODO: Handle negative numbers
        ! y = _lfortran_rep(z, -1)
    end subroutine
end module

program main
    use rep_test_mod
    implicit none
    call f()
end program
