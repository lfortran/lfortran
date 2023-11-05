double precision function f(x)
  double precision x
  f = x**2
  return
end function f

subroutine sub(f)
  external f
  double precision f
  print *, f(2.0d0)
  if (abs(f(2.0d0) - 4.0d0) > 1.0d-10) error stop
  return
end subroutine sub

recursive subroutine a()
external f 
call sub(f)
end

program external_07
  call a()
end
