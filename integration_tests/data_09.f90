program data_09
    integer vect(3350), i
    data vect /3350*2/
    
    do i = 1, 3350
        if (vect(i) /= 2) error stop
    end do
end program
