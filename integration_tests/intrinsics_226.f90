program intrinsics_226

    implicit none
    integer(1) :: a1(2)
    integer(4) :: b1(5)
    integer(8) :: c1(10)
  
    integer(1), parameter :: a2 = iall([1, 2, 3, 4, 5])
    integer(4), parameter :: b2 = iall([3, 5, 7, 9, 11])
    integer(8), parameter :: c2 = iall([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
    a1 = [3, 5]
    b1 = [3, 5, 7, 9, 11]
    c1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
    print*, iall(a1)
    if (iall(a1) /= 1) error stop
    print*, iall(b1)
    if (iall(b1) /= 1) error stop
    print*, iall(c1)
    if (iall(c1) /= 0) error stop
    
    print*, a2
    if (a2 /= 0) error stop
    print*, b2
    if (b2 /= 1) error stop
    print*, c2
    if (c2 /= 0) error stop
  
end program
  