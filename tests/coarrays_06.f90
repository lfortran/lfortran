module m
    integer :: x[*]
contains
    subroutine show()
        print *, x[2]
    end subroutine
end module

program p
    use m
    call show()
    sync all
end program
