program rank_01
    ! gfortran folds each of these arguments of rank to a constant and accepts it
    implicit none
    integer, parameter :: n(4) = 1
    logical, parameter :: l(2) = .true.
    integer :: a(2, 3)
    if (rank(n + 1) /= 1) error stop
    if (rank(.not. l) /= 1) error stop
    if (rank(lbound(a) + 1) /= 1) error stop
    print *, "ok"
end program
