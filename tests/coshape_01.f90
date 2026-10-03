program coshape_01
    implicit none

    integer :: x[*]

    if (any(coshape(x, kind=8) /= [2_8])) error stop
end program coshape_01