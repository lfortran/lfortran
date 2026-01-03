subroutine b(f,g)
    ! f, g are functions
    call f(1.39)
    call g(-9.16)
end subroutine

subroutine f(x)
    real :: x
    print *, x
    if (abs(x - 1.39) > 1e-8) error stop
end subroutine

subroutine g(x)
    real :: x
    print *, x
    if (abs(x - (-9.16)) > 1e-8) error stop
end subroutine

subroutine a()
interface
    subroutine f(x)
        real :: x
    end subroutine
    subroutine g(x)
        real :: x
    end subroutine
end interface
call b(f,g)
end subroutine

program external_08
call a()
end program
