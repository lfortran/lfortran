! `shape` of a `reshape` of an array constant, printed and used directly
! as an expression (#3520).
program arrays_reshape_57
    implicit none
    integer :: s(2)

    print *, shape(reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4]))

    s = shape(reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4]))
    if (s(1) /= 3) error stop 1
    if (s(2) /= 4) error stop 2

    if (any(shape(reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4])) /= [3, 4])) error stop 3
    if (size(shape(reshape([1, 2, 3, 4, 5, 6], [2, 3]))) /= 2) error stop 4
    if (product(shape(reshape([1, 2, 3, 4, 5, 6], [2, 3]))) /= 6) error stop 5
end program arrays_reshape_57
