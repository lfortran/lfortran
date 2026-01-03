program intrinsics_382
  implicit none

  real :: input(2)
  real :: output(2)  
  input = [1, 2]
  call forward(input, output)
  if (any(output /= [1, 2])) error stop
contains

  subroutine forward(input, output)
    real, intent(inout) :: input(:)
    real, intent(inout) :: output(:)
    output = pack(input, .true.)  
  end subroutine forward

end program intrinsics_382