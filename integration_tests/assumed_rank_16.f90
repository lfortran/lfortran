program assumed_rank_16
  use, intrinsic :: iso_c_binding, only: c_int32_t, c_size_t, c_sizeof
  implicit none

  integer(c_int32_t) :: int_matrix(3, 4)

  call verify_bytes(int_matrix, c_sizeof(int_matrix))

  print *, "SUCCESS: c_sizeof check passed!"
contains

  subroutine verify_bytes(arg, expected_bytes)
    implicit none

    integer(c_int32_t), intent(in), dimension(..) :: arg
    integer(c_size_t), intent(in)                  :: expected_bytes
    integer(c_size_t)                              :: actual_bytes

    actual_bytes = c_sizeof(arg)

    print '(A, I0, A, I0, A)', "Actual: ", actual_bytes, " bytes | Expected: ", expected_bytes, " bytes"

    if (actual_bytes /= expected_bytes) then
      print *, "ERROR: Size mismatch detected!"
      error stop 1
    end if
  end subroutine verify_bytes
end program assumed_rank_16
