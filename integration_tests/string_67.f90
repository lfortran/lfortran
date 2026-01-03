! Test out calls to `achar` (Function returing a character) to make sure it doesn't fill the stack
program string_67

    integer :: j
    
    character(4) :: ssss
    integer :: i
    j = 1
  do i =0, 65536
    ssss = achar(97+i) 
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
    ssss = achar(97+j)
  end do 
  end program
  