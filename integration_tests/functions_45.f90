! Test calling a string-returing function in a loop
program functions_45
    integer :: i
    integer :: n_slashes
    n_slashes = 20
    do i=1 , 10
        print *, repeat('\', n_slashes / 2)
    end do
end program
    