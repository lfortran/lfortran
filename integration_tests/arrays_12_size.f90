module arrays_12_size_mod
  contains 
  subroutine sub(xx)
    real, intent(in) :: xx(:)
    real :: buggy(int(2.0*count(xx > 0.0)))
    print * , size(buggy)
    if (size(buggy) /= 6) error stop
  end subroutine sub
end module arrays_12_size_mod
program arrays_12_size
  use arrays_12_size_mod
  real :: xx(5) = [1.1, 2.1, -1.3, 3.4, 0.0]
  call sub(xx)
end program arrays_12_size
