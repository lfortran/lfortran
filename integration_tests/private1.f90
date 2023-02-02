module foo1
    private
end module

module foo2
    real :: y = 2
end module

program test
    use foo2
    print *, y
end program
