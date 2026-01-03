program main
    implicit none
    real :: x(2)
    call compare_solutions(x)

    contains
    subroutine compare_solutions(x)
    real,dimension(:),intent(in) :: x
    real:: diff(2)
    diff = [1.0, 2.0]
    write (*,*), diff
    
    end subroutine compare_solutions

end program main
