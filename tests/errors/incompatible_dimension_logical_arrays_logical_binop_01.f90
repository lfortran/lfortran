program main
   logical, parameter :: x1(3) = [.true., .false., .false.]
   print *, x1 .neqv. [.true., .true.]
end program
