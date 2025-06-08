program intrinsics_345
    implicit none
    integer, dimension(2, 3) :: array1 = reshape([1, 1, 0, 1, 5, 1], [2, 3])
    integer, dimension(2, 3) :: array2 = reshape([2, 1, 0, 1, 5, 1], [2, 3])
    integer, dimension(2, 3) :: array3 = reshape([2, 2, 0, 1, 5, 1], [2, 3])
    integer, dimension(2, 3) :: array4 = reshape([1, 1, 1, 1, 1, 1], [2, 3])
    integer, dimension(2, 3) :: array5 = reshape([2, 2, 1, 0, 1, 0], [2, 3])
    integer, dimension(2, 3) :: array6 = reshape([0, 1, 1, 1, 1, 2], [2, 3])
    integer :: value = 1

    if (any(findloc(array1, value, dim=1) /= [1, 2, 2])) error stop
    if (any(findloc(array1, value, dim=1, back=.true.) /= [2, 2, 2])) error stop
    if (any(findloc(array1, value, dim=2) /= [1, 1])) error stop
    if (any(findloc(array1, value, dim=2, back=.true.) /= [1, 3])) error stop
    if (any(findloc(array2, value, dim=1) /= [2, 2, 2])) error stop
    if (any(findloc(array2, value, dim=1, back=.true.) /= [2, 2, 2])) error stop
    if (any(findloc(array2, value, dim=2) /= [0, 1])) error stop
    if (any(findloc(array2, value, dim=2, back=.true.) /= [0, 3])) error stop
    if (any(findloc(array3, value, dim=1) /= [0, 2, 2])) error stop
    if (any(findloc(array3, value, dim=1, back=.true.) /= [0, 2, 2])) error stop
    if (any(findloc(array3, value, dim=2) /= [0, 2])) error stop
    if (any(findloc(array3, value, dim=2, back=.true.) /= [0, 3])) error stop
    if (any(findloc(array4, value, dim=1) /= [1, 1, 1])) error stop
    if (any(findloc(array4, value, dim=1, back=.true.) /= [2, 2, 2])) error stop
    if (any(findloc(array4, value, dim=2) /= [1, 1])) error stop
    if (any(findloc(array4, value, dim=2, back=.true.) /= [3, 3])) error stop
    if (any(findloc(array5, value, dim=1) /= [0, 1, 1])) error stop
    if (any(findloc(array5, value, dim=1, back=.true.) /= [0, 1, 1])) error stop
    if (any(findloc(array5, value, dim=2) /= [2, 0])) error stop
    if (any(findloc(array5, value, dim=2, back=.true.) /= [3, 0])) error stop
    if (any(findloc(array6, value, dim=1) /= [2, 1, 1])) error stop
    if (any(findloc(array6, value, dim=1, back=.true.) /= [2, 2, 1])) error stop
    if (any(findloc(array6, value, dim=2) /= [2, 1])) error stop
    if (any(findloc(array6, value, dim=2, back=.true.) /= [3, 2])) error stop
end program
