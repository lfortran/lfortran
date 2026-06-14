program pointer_remapping_01
    implicit none
    integer, target :: values(4)
    integer, pointer :: p(:)
    
    values = [10, 20, 30, 40]
    p(5:) => values(2:4)
    
    print *, size(p)
    print *, p(5)
    print *, p(6)
    print *, p(7)
    
    if (size(p) /= 3) error stop
    if (p(5) /= 20) error stop
    if (p(6) /= 30) error stop
    if (p(7) /= 40) error stop
end program
