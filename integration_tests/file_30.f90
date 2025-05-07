subroutine file_write(iu, i)
  integer :: i, iu(5)
  WRITE(IU(I), *) 'xyzzy'
end subroutine

program file_30
  implicit none
  integer :: iu(5), ios
  character(len=10) :: line
  character(len=25) :: fname

  fname = 'temp_output_file_30.txt'
  iu(3) = 20  

  open(unit=iu(3), file=fname, status='replace', action='write')
  call file_write(iu, 3)
  close(iu(3))

  open(unit=99, file=fname, status='old', action='read', iostat=ios)
  read(99, '(A)', iostat=ios) line
  close(99)

  print *, trim(line)
  if (trim(adjustl(line)) /= 'xyzzy') error stop

  open(unit=97, file=fname, status='old')
  close(97, status='delete')

end program
