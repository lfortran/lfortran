      program fixed_form_io_keyword_02
         implicit none
         character(len=1) :: seq
         character(len=5) :: str
         open(10, file='file_01_data.txt')
         read(10, *) str
         print *, 'str =', str
         if (str /= "10130") error stop 1
         INQUIRE(UNIT=10,SEQUENTIAL=seq)
         print *, 'seq =', seq
         REWIND(UNIT=10)
         FLUSH(UNIT=10)
      end program fixed_form_io_keyword_02
