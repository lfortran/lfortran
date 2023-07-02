subroutine sub1()
   double precision el
   common /dvod01/ el(13)
end

subroutine sub2()
   double precision el
   common /dvod01/ el(13)
end

program common_11
   call sub1()
   call sub2()
end program
