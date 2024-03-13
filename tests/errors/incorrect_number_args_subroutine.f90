program expr
    implicit none
    call my_func(1)

contains
    subroutine my_func()
        print *, "hi"
    end subroutine
end program expr
