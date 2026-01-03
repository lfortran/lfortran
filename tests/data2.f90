program main
    call a()
end program

subroutine a()
    double precision factor, zero
    data factor,zero /1.0d2,0.0d0/
    print *, "factor in a:", factor
    call b(factor)

end subroutine

subroutine b(factor)
    double precision factor
    print *, "factor: ", factor
end subroutine