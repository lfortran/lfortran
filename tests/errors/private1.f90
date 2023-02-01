module foo1

    private

    real :: x = 1

end module

module foo2

    use foo1

    real :: y = 2

end module

program test

    use foo2

    x = 5
    write(*,*) x, y

end program
