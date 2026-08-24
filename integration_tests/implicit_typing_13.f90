subroutine dist
end subroutine

subroutine calculate
    dist = 1.0
    if (dist /= 1.0) error stop
end subroutine

program implicit_typing_13
    call calculate
end program
