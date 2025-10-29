module integertostring
  implicit none
  integer :: n(2) = [42,-1]
contains
  pure function i0(n)
    character(:),allocatable :: i0
    integer,intent(in)       :: n
    character(range(n)+2)    :: result
    write(result,"(I0)") n ! write(i0, ...) with i0 unallocated is bad f2018
    i0 = trim(adjustl(result))
  end function i0
end module integertostring
program print_arr_08
! Does gfortran warn when it shouldn't with an allocatable scalar?
  use integertostring, only: i0, n
  implicit none
  character(:),allocatable:: string
  string = 'The answer is '//merge(i0(n(1)),i0(n(2)),n(1)>0)//'.'
  print *,'"'//string//'"'
end program print_arr_08