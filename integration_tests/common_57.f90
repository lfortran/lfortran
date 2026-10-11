! The values that BLOCK DATA in another file gives to blank COMMON and to a
! named block are seen through other names, also through a module.
program common_57
    use common_57_mod, only: first, rest
    implicit none
    real :: x, y
    common x, y
    print *, x, y, first, rest
    if (abs(x - 1.5) > 1.0e-6) error stop
    if (abs(y - 2.5) > 1.0e-6) error stop
    if (first /= 3) error stop
    if (any(rest /= [0, 4])) error stop
end program common_57
