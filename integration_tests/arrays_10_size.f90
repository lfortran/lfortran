module mod_arrays_10_size
  contains 
  subroutine OBJ(x)
    real, intent(inout) :: x(:)
 end subroutine
  subroutine sub(calfun, yy, xx)
    procedure(OBJ) :: calfun
    integer, intent(in) :: yy
    real, intent(in) :: xx(:)
    real :: buggy(2*count(xx > 0.0))
    print * , size(buggy)
    if (size(buggy) /= 6) error stop
  end subroutine sub
end module mod_arrays_10_size
program arrays_10_size
  use mod_arrays_10_size
  real :: xx(5) = [1.1, 2.1, -1.3, 3.4, 0.0]
  call sub(OBJ ,2, xx)
end program arrays_10_size
