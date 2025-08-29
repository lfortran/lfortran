program bindc_07
  use iso_c_binding, only: c_char, c_ptr, c_null_ptr, c_size_t, c_int, c_associated, c_f_pointer
  implicit none

  interface
     function test_getcwd(buf, size) result(res) bind(c, name="getcwd_dummy")
       import c_char, c_size_t, c_ptr
       character(kind=c_char,len=1), intent(out) :: buf
       integer(c_size_t), value, intent(in) :: size
       type(c_ptr) :: res
     end function test_getcwd

     function test_char_array(buf, len) result(res) bind(c, name="test_char_array")
      import c_char, c_int, c_ptr
      character(kind=c_char), dimension(*), intent(inout) :: buf
      integer(c_int), value, intent(in) :: len
      type(c_ptr) :: res
   end function test_char_array
  end interface

  ! Test variables
  character(len=1024) :: large_result
  character(len=1) :: small_result(256)
  character(len=50) :: medium_result
  type(c_ptr) :: ptr_result
  integer :: i
  type(c_ptr) :: res_ptr

  print *, "=== ROBUST BIND(C) CHARACTER ARRAY TEST ==="
  print *, ""

  ! Test 1: Large character variable
  print *, "TEST 1: Large character variable"
  large_result = repeat(' ', len(large_result))
  ptr_result = test_getcwd(large_result, int(len(large_result), c_size_t))
  
  if (c_associated(ptr_result)) then
     print *, "SUCCESS: Large buffer test passed"
     print *, "Result: '", trim(large_result), "'"
  else
     print *, "FAILED: Large buffer test failed"
  end if
  print *, ""

  ! Test 2: Array of single characters

  print *, "TEST 2: Array of single characters"
  small_result = ' '
  res_ptr = test_char_array(small_result, int(size(small_result), c_int))

  if (c_associated(res_ptr)) then
    print *, "SUCCESS: Small array test passed"
    write(*,'(A)', advance='no') "Result: '"
    do i = 1, min(size(small_result), 50)
        if (small_result(i) == char(0)) exit
        write(*,'(A)', advance='no') ,small_result(i)
    end do
    write(*,'(A)') "'"
  else
    print *, "FAILED: Small array test failed"
  end if


  ! Test 3: Medium character variable
  print *, "TEST 3: Medium character variable"
  medium_result = repeat(' ', len(medium_result))
  ptr_result = test_getcwd(medium_result, int(len(medium_result), c_size_t))

  if (c_associated(ptr_result)) then
     print *, "SUCCESS: Medium buffer test passed"
     print *, "Result: '", trim(medium_result), "'"
  else
     print *, "FAILED: Medium buffer test failed"
  end if

  print *, ""
  print *, "=== ALL TESTS COMPLETED ==="

end program bindc_07