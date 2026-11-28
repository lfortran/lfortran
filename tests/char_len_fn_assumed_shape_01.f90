module char_len_fn_assumed_shape_01_m
contains
  pure integer function f(a)
    character(len=*), intent(in) :: a(:)
    f = size(a)
  end function
  function g(a) result(r)
    character(len=*), intent(in) :: a(:)
    character(len=f(a)) :: r
    r = ''
  end function
end module

program char_len_fn_assumed_shape_01
  use char_len_fn_assumed_shape_01_m, only: g
  implicit none
  character(len=5) :: a(2) = ['two  ', 'three']
  print *, len(g(a))
end program
