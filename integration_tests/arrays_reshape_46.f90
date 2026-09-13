subroutine test_reshape_2d(original, reshaped)
  implicit none
  integer, intent(in) :: original(2,2)
  integer, intent(out) :: reshaped(2,2)
  reshaped = reshape(original, shape=[2,2], order=[2,1])
end subroutine test_reshape_2d

subroutine test_reshape_1d(source, reshaped)
  implicit none
  integer, intent(in) :: source(4)
  integer, intent(out) :: reshaped(2,2)
  reshaped = reshape(source, [2,2], order=[2,1])
end subroutine test_reshape_1d

program arrays_reshape_46
    ! Test that the order argument of reshape is honored when the source
    ! array is a dummy argument (issue #12655).
    implicit none
    integer :: original(2,2) = reshape([1, 2, 3, 4], [2,2])
    integer :: reshaped(2,2)

    ! reshape with order in the main program
    reshaped = reshape(original, shape=[2,2], order=[2,1])
    if (sum(abs(reshaped - original)) /= 2) error stop

    ! same reshape inside a subroutine (source is a dummy argument)
    reshaped = 0
    call test_reshape_2d(original, reshaped)
    if (sum(abs(reshaped - original)) /= 2) error stop
    if (any(reshaped /= reshape([1, 3, 2, 4], [2,2]))) error stop

    ! 1d dummy source
    reshaped = 0
    call test_reshape_1d([1, 2, 3, 4], reshaped)
    if (any(reshaped /= reshape([1, 3, 2, 4], [2,2]))) error stop
end program arrays_reshape_46
