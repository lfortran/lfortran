program arrays_reshape_24
    implicit none
    integer :: arr(4) = [1, 2, 3, 4]
    integer :: reshaped(2, 3)
    integer :: pad(1) = [5]
    reshaped = reshape(arr, shape=[2, 3], pad = pad)
    print *, reshaped
    if (any(reshaped /= reshape([1, 2, 3, 4, 5, 5], shape=[2, 3]))) error stop
end program arrays_reshape_24