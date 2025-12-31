program pp
  call ss(ff(f(), 20))
  contains 
  function ff(x, y) result(str)
    integer :: x
    integer :: y
    integer :: str(x)
  end function
  function f() result(ret)
    integer :: ret
    ret = 10
    print *, "called"
  end function
  subroutine ss(x)
    integer :: x(*)
  end subroutine 
end program 