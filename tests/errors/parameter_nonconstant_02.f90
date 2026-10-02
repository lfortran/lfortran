program parameter_nonconstant_02
    integer :: bla
    integer, parameter :: y = bla + 1
    print *, y
end program
