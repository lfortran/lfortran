subroutine prja()
integer iownd, iowns
common /ls0001/ iownd(14), iowns(6)
end

program common_08
    call prja()
end program
