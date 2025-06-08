module mod_arrays_09_size
  contains 
  subroutine sub(xx)
    real :: xx(5)
    real :: buggy(count(xx > 0.0))
    if (size(buggy) /= 3) error stop
  end subroutine sub
end module mod_arrays_09_size
program arrays_09_size
  use mod_arrays_09_size
  real :: xx(5) = [1.1, 2.1, -1.3, 3.4, 0.0]
  call sub(xx)
end program