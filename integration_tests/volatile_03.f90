program volatile_03
    implicit none
    character, volatile :: x 
    character :: y = '2'
    y = x
end program volatile_03