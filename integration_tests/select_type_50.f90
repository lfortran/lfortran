module select_type_50_m
  implicit none
  type :: array_type
    real, allocatable :: val(:,:)
  end type
contains
  function get_n(input) result(n)
    class(*), dimension(..), intent(in) :: input
    integer :: n
    n = 0
    select rank(input)
    rank(1)
      select type(input)
      class is(array_type)
        n = size(input(1)%val, 2)
      end select
    end select
  end function
end module

program select_type_50
  use select_type_50_m
  implicit none
  type(array_type) :: arr(1)
  allocate(arr(1)%val(3, 5))
  arr(1)%val = 1.0
  if (get_n(arr) /= 5) error stop
  print *, "PASSED"
end program
