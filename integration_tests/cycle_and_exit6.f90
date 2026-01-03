program cycles_stuck
implicit none 

integer :: iiter, maxit 
maxit = 5 

do iiter = 1, maxit 
    if ( .true. ) then 
        cycle 
        print *, "inside loop"
    end if 
end do 

print *, "End of program."

end 
