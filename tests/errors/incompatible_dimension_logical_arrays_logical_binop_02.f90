program main
    logical :: x1(2, 2)
    x1(1, 1) = .true.
    x1(1, 2) = .false.
    x1(2, 1) = .false.
    x1(2, 2) = .true.
    print *, x1 .neqv. [.true., .true.]
 end program
