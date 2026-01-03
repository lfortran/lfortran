module foo1
    private
end module

module foo2
    real :: y = 2

contains

    subroutine f()
        y = 3.0
    end subroutine f
end module

program test
    use foo2
    print *, y
    if( abs(y - 2.0) > 1e-12 ) error stop

    call f()
    print *, y
    if( abs(y - 3.0) > 1e-12 ) error stop
end program
