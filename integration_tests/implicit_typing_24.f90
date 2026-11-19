subroutine bug()
    allocatable wt(:)
    if (.not. allocated(wt)) allocate(wt(10))
    wt(1) = 3.0
    if (abs(wt(1) - 3.0) > 1e-6) error stop
    wt(10) = 1.0
    if (abs(wt(10) - 1.0) > 1e-6) error stop
end subroutine

program implicit_typing_24
    call bug()
    print *, "ok"
end program
