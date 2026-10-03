module achar_transfer_mod
  implicit none
  public iascii, ascii
  interface ascii
     module procedure asciiarray, asciiscalar
  end interface ascii
contains
  pure function iascii(string)
    character, intent(in) :: string*(*)
    integer :: iascii(len(string))
    iascii = iachar(transfer(string, (/'A'/)))
  end function iascii

  pure function asciiarray(codes)
    integer, intent(in) :: codes(:)
    character :: asciiarray*(size(codes))
    asciiarray = transfer(achar(codes), asciiarray)
  end function asciiarray

  pure function asciiscalar(codes)
    integer, intent(in) :: codes
    character :: asciiscalar*1
    asciiscalar = asciiarray((/codes/))
  end function asciiscalar
end module achar_transfer_mod

program transfer_15
  use achar_transfer_mod, only: iascii, ascii
  implicit none
  integer :: empty(0), iarr_empty(0)
  integer :: codes(3), iarr(3), iarr1(1)
  character(3) :: result_str

  codes = (/97, 98, 99/)
  result_str = ascii(codes)
  if (result_str /= 'abc') error stop

  if (ascii(97) /= 'a') error stop
  if (ascii((/97/)) /= 'a') error stop
  if (ascii(iascii('a')) /= 'a') error stop
  if (ascii(iascii('abc')) /= 'abc') error stop
  if (ascii(iascii('')) /= '') error stop
  if (ascii((/97,98,99/)) /= 'abc') error stop
  if (ascii(empty) /= '') error stop

  iarr1 = iascii('a')
  if (iarr1(1) /= 97) error stop

  iarr = iascii('abc')
  if (iarr(1) /= 97) error stop
  if (iarr(2) /= 98) error stop
  if (iarr(3) /= 99) error stop

  if (size(iascii('')) /= 0) error stop

  iarr_empty = iascii(ascii(empty))
  if (size(iarr_empty) /= 0) error stop

  iarr = iascii(ascii((/1,2,3/)))
  if (iarr(1) /= 1) error stop
  if (iarr(2) /= 2) error stop
  if (iarr(3) /= 3) error stop

  print *, "All tests passed."
end program transfer_15
