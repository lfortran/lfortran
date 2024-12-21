! Issue 4649: reshape called in shape parameter of reshape function 
program main
    implicit none

    complex(8) :: my_complex(2, 3)
    my_complex = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], reshape([2, 3], [2]))
    print *, my_complex
end program