program main
  implicit none
  real :: input(2)
  input = [1, 2]
  call solve(input)
contains

  subroutine solve(input)
    real :: input(2)
    print*, minloc(input, 1, .true.)  
    print*, maxloc(input, 1, .true.)
  end subroutine
end program
