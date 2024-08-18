program intrinsics_301
    implicit none
    call rand()
    
    contains
    
        subroutine rand()
        real :: u
        integer :: index
        do index = 1, 5
            call random_number(u)
            print *, u
        end do
        end subroutine
end program