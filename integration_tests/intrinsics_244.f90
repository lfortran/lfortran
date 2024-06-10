program intrinsics_244
    integer, parameter :: x1(4) = cshift([1, 2, 3, 4], 2)
    real, parameter :: y1(4) = cshift([1.0, 2.0, 3.0, 4.0], -2)
    logical, parameter :: z1(4) = cshift([.true., .false., .true., .false.], -1)
    character(len=2), parameter :: c1(4) = cshift(["ab", "bc", "cd", "de"], 2)

    integer :: x(4) = [1, 2, 3, 4]
    real :: y(4) = [1.0, 2.0, 3.0, 4.0]
    logical :: z(4) = [.true., .false., .true., .false.]
    character(len=2) :: c(4) = ["ab", "bc", "cd", "de"]
    integer :: shift(4) = [1, -2, -3, 2]
    integer :: shift_val = -2

    print*, cshift(x, shift(1))
    if (any(cshift(x, shift(1)) /= [2, 3, 4, 1])) error stop
    print*, cshift(y, shift(2))
    if (any(cshift(y, shift(2)) /= [3.0, 4.0, 1.0, 2.0])) error stop
    print*, cshift(z, shift(3))
    if (any(cshift(z, shift(3)) .neqv. [.false., .true., .false., .true.])) error stop
    print*, cshift(c, shift(4))
    if (any(cshift(c, shift(4)) /= ["cd", "de", "ab", "bc"])) error stop
  
    print*, x1
    if (any(x1 /= [3, 4, 1, 2])) error stop
    print*, y1
    if (any(y1 /= [3.0, 4.0, 1.0, 2.0])) error stop
    print*, z1
    if (z1(1) .neqv. .false. .or. z1(2) .neqv. .true. .or. z1(3) .neqv. .false. .or. z1(4) .neqv. .true.) error stop

    print *, cshift(x, shift_val)
    if (any(y1 /= [3.0, 4.0, 1.0, 2.0])) error stop

    print *, shift_val
    if (shift_val /= -2) error stop
    ! print*, c1 ! gives segfault

end program
