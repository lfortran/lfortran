module foo2
    private
    real :: y = 2
end module
    
program test
    use foo2, only: y
    print *, y
end program
