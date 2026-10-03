program transfer_29
  implicit none

  type :: pair
    integer :: fst
    real :: snd
  end type

  type(pair) :: original(2)
  type(pair) :: result_val(2)
  integer(1) :: bytes(16)

  original(1) = pair(1, 53.)
  original(2) = pair(3, 47.)
  bytes = transfer(original, bytes)

  result_val = transfer(bytes, original(1), 2)

  if (result_val(1)%fst /= 1) error stop
  if (abs(result_val(1)%snd - 53.) > 0.01) error stop
  if (result_val(2)%fst /= 3) error stop
  if (abs(result_val(2)%snd - 47.) > 0.01) error stop
  print *, "PASS"
end program
