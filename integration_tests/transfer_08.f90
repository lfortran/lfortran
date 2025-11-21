program transfer_08
    implicit none
  
    character(len=:), allocatable :: lhs
    character(kind=1) :: rhs(3)
  
    rhs = ["A", "B", "C"]
  
    ! Transfer first 2 characters of rhs into lhs
    allocate(character(len=2) :: lhs)
    lhs(1:2) = transfer(rhs(1:2), lhs)
  
    print *, "lhs = [", lhs, "]"
    print *, "rhs = [", rhs, "]"
  
end program