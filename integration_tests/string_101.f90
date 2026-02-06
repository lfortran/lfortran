! Test allocatable character return from function
! Related to issue #6725
program string_101
  implicit none
  character(:), allocatable :: result
  result = get_string()
  if (result /= 'hello') error stop
contains
  function get_string() result(s)
    character(:), allocatable :: s
    s = 'hello'
  end function
end program
