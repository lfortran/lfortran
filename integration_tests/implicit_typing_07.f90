program implicit_do_combined
    integer :: n(3), m(3)
    integer :: p(3) = [(42, i = 1, 3)]  ! declaration initialization
    
    n = [(42, i = 1, size(n))]  ! array constructor with size()
    
    do i = 1, size(m)  ! explicit do loop
        m(i) = 42
    end do
    
    if (any(n /= 42)) error stop "n failed"
    if (any(m /= 42)) error stop "m failed"
    if (any(p /= 42)) error stop "p failed"
    
    print "(3I3)", n
end program
