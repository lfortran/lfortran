program intrinsics_353
    implicit none
    real, dimension(8) :: x  
    real :: y

    call random_number(harvest=x)
    print *, 'Random numbers:', x

    call random_number(harvest=y)
    print *, 'Random numbers:', y
end program