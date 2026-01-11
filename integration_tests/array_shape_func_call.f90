program array_shape_func_call
  integer :: call_count = 0

  call ss(ff(f(), 20))

  if (call_count /= 1) then
    error stop "f() called more than once"
  end if

contains

  function ff(x, y) result(str)
    integer :: x
    integer :: y
    integer :: str(x)
  end function

  function f() result(ret)
    integer :: ret
    call_count = call_count + 1
    ret = 10
  end function

  subroutine ss(x)
    integer :: x(*)
  end subroutine

end program array_shape_func_call
