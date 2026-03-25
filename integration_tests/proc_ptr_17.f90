function isquare(n)
    integer, intent(in) :: n
    integer :: isquare
    isquare = n * n
end function

program proc_ptr_17
    pointer :: ifp
    external :: ifp, isquare

    ifp => isquare
    if (ifp(7) /= 49) error stop
    print *, ifp(7)
end program
