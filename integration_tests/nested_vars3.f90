module nested_vars3
    implicit none

    contains

    subroutine h()
        integer, parameter :: x = 10
        call g()

        contains

        subroutine g()
            print *, x
            if (x /= 10) error stop
        end subroutine

    end subroutine

end module


program nested_vars3_main
    use nested_vars3, only: h
    implicit none
    call h()
end program
