module mod_arrays_11_size
  contains 
  subroutine sub(xx)
    real, intent(in) :: xx(:)
    real :: buggy(count(xx > 0.0))
    print * , size(buggy)
    if (size(buggy) /= 3) error stop
  end subroutine sub
end module mod_arrays_11_size
program arrays_11_size
  use mod_arrays_11_size
  real :: xx(5) = [1.1, 2.1, -1.3, 3.4, 0.0]
  call sub(xx)
end program arrays_11_size
