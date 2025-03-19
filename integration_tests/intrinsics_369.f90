program intrinsics_369
    integer :: x(1) = minloc([1, 2], .true.)
    integer :: y(1) = maxloc([1, 2], .true.)
    if (any(x /= 1)) error stop
    if (any(y /= 2)) error stop
end program