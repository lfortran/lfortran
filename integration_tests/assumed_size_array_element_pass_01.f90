! Test for gh-10347: passing element from assumed-size array
! to routine expecting assumed-size array should compile without crash
module test_mod
  contains

  subroutine inner_routine(x)
    real :: x(*)
    ! Just a simple routine that accepts assumed-size array
    ! Don't access elements to avoid bounds checking issues
  end subroutine inner_routine

  subroutine outer_routine(w)
    real :: w(*)
    ! Pass element from assumed-size array to another routine
    ! This was causing crash in gh-10347 during codegen
    call inner_routine(w(5))
  end subroutine outer_routine

end module

program assumed_size_array_element_pass_01
  use test_mod
  implicit none
  real :: test_arr(20)
  
  test_arr = 1.0
  
  ! This call chain tests passing w(5) to inner_routine
  ! where both w and x are dimension(*)
  call outer_routine(test_arr)
  
  print *, 'pass'
  
end program
