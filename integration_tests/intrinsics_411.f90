module intrinsics_411_asciimod
  implicit none
  public iascii
contains 
  pure function  iascii(   string)
    character,intent(in):: string*(*)
    integer      iascii(len(string))
    iascii = iachar(transfer(string,(/'A'/))) 
  end function iascii
end module

program intrinsics_411
  use intrinsics_411_asciimod, only: iascii
  implicit none
  integer :: result(1), expected(1)
  integer :: result3(3), expected3(3)
  
  result = iascii('a')
  expected = (/97/)
  if (any(result /= expected)) error stop "iascii('a') failed"

  result3 = iascii('abc')
  expected3 = (/97, 98, 99/)
  if (any(result3 /= expected3)) error stop "iascii('abc') failed"
  if (size(iascii('')) /= 0) error stop "iascii('') size failed"
end program intrinsics_411
