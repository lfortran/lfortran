program main
    implicit none

    integer :: a(5) = [1, 2, 3, 4, 5]
    integer :: b = 130
    b = b + 41
    if (b > 1000) then
        print *, a
        error stop
    else
        print *, "Success"
    end if
end program
