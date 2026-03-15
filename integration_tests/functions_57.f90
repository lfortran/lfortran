program functions_57
  implicit none

  integer, parameter :: keys(16) =  &
    [503,  87, 512,  61, 908, 170, 897, 275,  &
     653, 426, 154, 509, 612, 677, 765, 703 ]

  integer, allocatable :: results(:)

  results = copy(keys)
  if (size(results) /= 16) error stop
  if (results(1) /= 503) error stop
  if (results(16) /= 703) error stop

contains

  function copy(a)
    integer, intent(in) :: a(:)
    integer :: copy(size(a))

    copy = a
  end function

end program
