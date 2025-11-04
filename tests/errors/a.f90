subroutine add(x, y)
    implicit none
    real, intent(in) :: x, y
    print *, x + y
    call add2()
    contains
        subroutine add2()
            implicit none
            print *, x + y + 2
        end subroutine
end subroutine

program main
    implicit none
    call add(1.0, 2.0)
end program
