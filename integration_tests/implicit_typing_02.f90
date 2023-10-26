module implicit_typing_02_mod
    implicit none

    public bisect

    contains
    integer function bisect() result(c)
        c = 10
    end function
end module

program implicit_typing_02
    use implicit_typing_02_mod, only: bisect
    print *, bisect()
    if (bisect() /= 10) error stop
end program
