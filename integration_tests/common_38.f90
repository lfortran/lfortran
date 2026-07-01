program common_38
    implicit none
    call test()
    print *, "OK"
end program

subroutine test()
   implicit none
   integer, parameter :: n1=40, n2=1, ng=4
   integer, parameter :: l1 = 1-ng, m1 = n1+ng
   integer, parameter :: l2 = 1-ng, m2 = n2+ng
   integer :: arr
   common /cname/ arr(l1:m1, l2:m2, 12)
   
   arr(-3, -3, 1) = 42
   if (arr(-3, -3, 1) /= 42) error stop
end subroutine test
