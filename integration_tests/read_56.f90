program read_56
    implicit none
    character(20) :: string = '12345678901234567890'
  
    string(1:8) = 'ABCDEFGH'
    if (string /= 'ABCDEFGH901234567890') error stop
  
    string(5:12) = 'XXXXXXXX'
    if (string /= 'ABCDXXXXXXXX34567890') error stop
  end program read_56