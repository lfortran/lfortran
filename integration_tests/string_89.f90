! This test checks that assignment to a slice of an array 
! Inside Subroutine works correctly for assumed-length strings.
program string_89
  implicit none
  character(len=5)              :: strings(3) = ['-----','-----','-----']
  character(len=:), allocatable :: str(:)
  str   = ['ABC','BEE','SEE']
  call assign_slice(strings, str) 
  print *,strings
  if (strings(1) /= 'ABC') error stop
  if (strings(2) /= 'BEE') error stop
  if (strings(3) /= 'SEE') error stop

  contains 
  subroutine assign_slice(strings,str)
    implicit none
    character(len=*), intent(inout) :: strings(:)
    character(len=:), allocatable, intent(in) :: str(:)
    strings(:) = str
  end subroutine
end program string_89