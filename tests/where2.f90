subroutine where2
    implicit none
    integer a(10)
    integer b(10)
    
    where (a > b) 
        a = 1
    endwhere
    
    where(a == 1)
        a = 2
    else where(a == 2)
        a = 3
    else where
        a = a * 2
    endwhere
end subroutine

program main
call where2
end program
