program generic_name_02
    call p("abc")
    contains

    subroutine p(a)
        Class(*), intent(in) :: a
        print *, "Hello World"
    end subroutine

end program
