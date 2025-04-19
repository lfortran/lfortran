subroutine check1()
   integer incx
   common /com/incx
   do 80 incx = 1, 2
80 continue
   return
end subroutine

subroutine stest()
   integer incx, i
   common /com/incx
   do 40 i = 1, 5
40 continue
   return
end subroutine

program doloop_13
   integer incx
   common /com/incx

   call check1()
   call stest()
   if (incx /= 3) error stop
end program
