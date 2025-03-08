subroutine outer()
   print *, f(2.0_8)
   if ( abs(f(2.0_8) - 14.928527864588919) > 1e-7 ) error stop
contains
   function f(r)
      implicit real(kind=8) (f, r, n)
      n = 3.9_8
      f = r**n
   end function f
end subroutine outer

program implicit_typing_05
call outer()
end program implicit_typing_05
