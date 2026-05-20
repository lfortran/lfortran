program entry_17
    implicit none
    integer :: a(4)
    a = 0
    call ent(a)
    if (a(1) /= 10) error stop
    if (a(2) /= 20) error stop
    if (a(3) /= 30) error stop
    if (a(4) /= 40) error stop
    print *, a
end program

subroutine sub
    integer :: iarr(4)
    return
    entry ent(iarr)
    iarr(1) = 10
    iarr(2) = 20
    iarr(3) = 30
    iarr(4) = 40
    return
end subroutine
