subroutine s()
    implicit none
    type :: t
        integer :: h = 0
    end type
    type(t) :: v
    print *, v%h
end subroutine

program main
    implicit none
    interface
        subroutine s()
        end subroutine s
    end interface
    call s()
end program
