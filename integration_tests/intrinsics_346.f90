module asciimod ! f95-compliant file asciiB.f90: [] only in character strings
    implicit none
    public iascii,ascii
    interface ascii
       module procedure asciiarray,asciiscalar
    end interface ascii
  contains 
    pure function iascii(    string)
      character,intent(in):: string*(*)
      integer     iascii(len(string)), i
      do i = 1, len(string)
         iascii(i) = iachar(string(i:i))
      end do
    end function iascii
  
    pure function asciiarray(      codes)
      integer,intent(in)::         codes(:)
      character   asciiarray*(size(codes))
      integer i
      do i = 1, size(codes)
         asciiarray(i:i) = achar(codes(i))
      end do
    end function asciiarray
  
    pure function asciiscalar(codes) 
      integer,intent(in)::    codes
      character   asciiscalar*1
      asciiscalar = asciiarray((/codes/))
    end function asciiscalar
  
end module asciimod
program intrinsics_346
    use asciimod
    implicit none

    ! Variables
    character(len=5) :: inputString
    integer, allocatable :: asciiCodes(:)
    character(len=:), allocatable :: outputString

    inputString = "Hi!"
    allocate(asciiCodes(len(inputString)))
    asciiCodes = iascii(inputString)

    print *, "ASCII codes for '", inputString, "':", asciiCodes
    if (any(asciiCodes /= [72, 105, 33, 32, 32])) error stop

    outputString = ascii(asciiCodes)

    print *, "Converted back to string:", outputString
    if (outputString /= "Hi!") error stop

end program