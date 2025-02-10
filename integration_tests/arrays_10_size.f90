module mod_ext_arrays_10_size
  real :: ext_var = 0.0 
end module 
module mod_arrays_10_size
  contains 
  subroutine OBJ(x)
    real, intent(inout) :: x(:)
 end subroutine
  subroutine sub(calfun, yy, xx)
    use mod_ext_arrays_10_size, only: ext_var
    procedure(OBJ) :: calfun
    integer, intent(in) :: yy
    real, intent(in) :: xx(:)
    real :: buggy(2*count(xx > 0.0))
    real :: buggy2(2*count(xx > ext_var))
    real :: buggy3(2*count(xx > -ext_var))
    print * , size(buggy)
    if (size(buggy) /= 6) error stop
    print * , size(buggy2)
    if (size(buggy2) /= 4) error stop
    print * , size(buggy3)
    if (size(buggy3) /= 8) error stop
  end subroutine sub
end module mod_arrays_10_size
program arrays_10_size
  use mod_arrays_10_size
  use mod_ext_arrays_10_size
  real :: xx(5) = [1.1, 2.1, -1.3, 3.4, 0.0]
  ext_var = 1.1
  call sub(OBJ ,2, xx)
end program arrays_10_size
