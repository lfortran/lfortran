module nested_vars3
    implicit none

    contains

    subroutine h(a, filename)
        integer, intent(in) :: a
        character(len=*), intent(in) :: filename
        integer, parameter :: x = 10
        call g()

        contains

        subroutine g()
            print *, x
            if (x /= 10) error stop
            print *, a
            if (a /= 5) error stop
            if (filename /= "xyz") error stop
        end subroutine

    end subroutine

end module


program nested_vars3_main
    use nested_vars3, only: h
    implicit none
    call h(5, "xyz")
end program
