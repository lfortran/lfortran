! Tests for read(text, *) (arr(i), i=1,n) with character arrays inside do-loops
! Case 1: FixedSizeArray
! Case 2: PointerArray - Subroutine arguments

program read_81
  implicit none

  integer, parameter :: SLEN = 20
  character(len=SLEN) :: fixed_arr(2)
  character(len=SLEN), allocatable :: alloc_arr(:)
  character(len=SLEN) :: text
  integer :: i
  text = "'hello' 'world'"

  ! Case 1: FixedSizeArray — arr is local fixed-size
  read(text, *) (fixed_arr(i), i = 1, 2)
  print *, fixed_arr(1), fixed_arr(2)
  if (trim(fixed_arr(1)) /= 'hello') error stop
  if (trim(fixed_arr(2)) /= 'world') error stop

  ! Case 2: PointerArray — pass fixed array to assumed-shape dummy
  allocate(alloc_arr(2))
  call read_into_dummy(text, alloc_arr, 2)
  print *, alloc_arr(1), alloc_arr(2)
  if (trim(alloc_arr(1)) /= 'hello') error stop
  if (trim(alloc_arr(2)) /= 'world') error stop

contains

  subroutine read_into_dummy(text, arr, n)
    character(len=*), intent(in)  :: text
    character(len=SLEN), intent(out) :: arr(:)
    integer, intent(in) :: n
    integer :: i

    read(text, *) (arr(i), i = 1, n)
  end subroutine read_into_dummy

end program read_81