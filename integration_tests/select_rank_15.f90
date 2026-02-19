module select_rank_15_mod
  implicit none

  type :: my_t
    integer :: val = 0
  end type

contains

  function process_all(items) result(res)
    type(my_t), intent(in) :: items(..)
    type(my_t) :: res
    select rank(items)
      rank(2)
        res = helper(reshape(items, shape=[size(items)]))
    end select
  contains
    function helper(arr) result(r)
      type(my_t), intent(in) :: arr(:)
      type(my_t) :: r
      r%val = size(arr)
    end function
  end function

end module

program select_rank_15
  use select_rank_15_mod
  implicit none
  type(my_t) :: arr(2,2), r
  r = process_all(arr)
  if (r%val /= 4) error stop
  print *, r%val
end program
