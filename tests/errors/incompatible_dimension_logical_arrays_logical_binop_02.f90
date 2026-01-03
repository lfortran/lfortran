program main
    logical, parameter :: x1(3) = [.true., .false., .false.]
    logical, parameter :: x2(2) = [.true., .true.]
    print *, x1 .neqv. x2
 end program
 