program intrinsics_225

    implicit none
    integer(1) :: a1(2)
    integer(4) :: b1(5)
    integer(8) :: c1(10)
  
    integer(1), parameter :: a2 = iany([1, 2, 3, 4, 5])
    integer(4), parameter :: b2 = iany([3, 5, 7, 9, 11])
    integer(8), parameter :: c2 = iany([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
    a1 = [3, 5]
    b1 = [3, 5, 7, 9, 11]
    c1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
    print*, iany(a1)
    if (iany(a1) /= 7) error stop
    print*, iany(b1)
    if (iany(b1) /= 15) error stop
    print*, iany(c1)
    if (iany(c1) /= 15) error stop
  
    print*, a2
    if (a2 /= 7) error stop
    print*, b2
    if (b2 /= 15) error stop
    print*, c2
    if (c2 /= 15) error stop
  
end program
  