program coarrays_32
    implicit none

    integer :: x[*]
    integer :: lc(1), uc(1)

    ! Without DIM
    lc = lcobound(x)
    uc = ucobound(x)

    if (lc(1) /= 1) then
        error stop "Incorrect LCOBOUND."
    end if

    if (uc(1) /= num_images()) then
        error stop "Incorrect UCOBOUND."
    end if

    ! With DIM
    if (lcobound(x, dim=1) /= 1) then
        error stop "Incorrect LCOBOUND with DIM."
    end if

    if (ucobound(x, dim=1) /= num_images()) then
        error stop "Incorrect UCOBOUND with DIM."
    end if

end program coarrays_32