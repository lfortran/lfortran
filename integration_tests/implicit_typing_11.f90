subroutine c1 (IDIMY, Y)
   integer idimy
   DIMENSION       Y(IDIMY,*)
   COMPLEX         Y

   print *, Y(1:IDIMY, 1)
   ! check that the values are correct
   if (Y(1,1) /= (1.0, 1.0)) then
      error stop "Error: Y(1,1) is not correct"
   else if (Y(2,1) /= (2.0, 2.0)) then
      error stop "Error: Y(2,1) is not correct"
   else if (Y(3,1) /= (3.0, 3.0)) then
      error stop "Error: Y(3,1) is not correct"
   else
      print *, "All values are correct"
   end if

END

subroutine test()
   implicit real(kind=8) (a-h,o-z), integer(kind=4) (i-n)
   parameter ( a = 5.d0  )
   parameter ( b = 2.d0  )
   parameter ( c = a / b )

   print *, "Value of c is:", c
   if (abs(c - 2.5d0) > 1d-10) then
      error stop "Error: c is not correct"
   else
      print *, "Value of c is correct"
   end if
end subroutine test



program implicit_typing_10
   integer idimy
   parameter (idimy=3)
   complex y(idimy,1)

   y(1:IDIMY,1) = [(cmplx(i,i), i=1,idimy)]

   call c1(idimy,y)
   call test()

end program

