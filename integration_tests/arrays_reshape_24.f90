program arrays_reshape_24
    implicit none
    integer :: arr1(4) = [1, 2, 3, 4]
    integer :: arr2(6) = [1, 2, 3, 4, 5, 6]
    real :: arr3(4) = [1.0, 2.0, 3.0, 4.0]

    integer :: pad1(1) = [5]
    real :: pad2(2) = [1.0, 2.0]
    character(len=1) :: pad3(2) = ['a', 'b']

    integer :: reshaped(2, 3)
    real :: reshaped_real(2, 3)
    character(len=1) :: reshaped_char(2, 2)

    reshaped = reshape(arr1, shape=[2, 3], pad = pad1)
    print *, reshaped
    if (any(reshaped /= reshape([1, 2, 3, 4, 5, 5], shape=[2, 3]))) error stop

    reshaped = reshape(arr2, shape=[2, 3], pad = pad1)
    print *, reshaped
    if (any(reshaped /= reshape([1, 2, 3, 4, 5, 6], shape=[2, 3]))) error stop

    reshaped_real = reshape(arr3, shape=[2, 3], pad = pad2)
    print *, reshaped_real
    if (any(reshaped_real /= reshape([1.0, 2.0, 3.0, 4.0, 1.0, 2.0], shape=[2, 3]))) error stop

    reshaped_char = reshape(pad3, shape=[2, 2], pad = pad3)
    print *, reshaped_char
    if (any(reshaped_char /= reshape(['a', 'b', 'a', 'b'], shape=[2, 2]))) error stop

end program arrays_reshape_24