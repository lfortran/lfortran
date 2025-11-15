program transfer_08
    implicit none

    character(len=:), allocatable :: lhs
    character(kind=1) :: rhs(3)

    rhs = ["A", "B", "C"]
    allocate(character(len=2) :: lhs)
    lhs(1:2) = transfer(rhs(1:2), lhs)

    if (lhs /= "AB") error stop
    if (any(rhs /= ["A", "B", "C"])) error stop
end program
