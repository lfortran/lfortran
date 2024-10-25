program character_07
   character:: c(6)*3 = 'ab'       ! the array is populated from character "ab " after padding
   character:: d(6)*4
   character:: e(6)*2

   character(len=5), parameter :: ar1(2) = ["abcdef", "ghijkl"]
   character(len=5), parameter :: ar2(2) = ["abc"//"def", "ghi"//"jkl"]
   
   integer :: i
   
   if (ar1(1) /= "abcde") error stop
   if (ar1(2) /= "ghijk") error stop

   if (ar2(1) /= "abcde") error stop
   if (ar2(2) /= "ghijk") error stop
   
   do i = 1, 6
      print *, c(i), len(c(i))
      if (c(i) /= "ab ") error stop
      if (len(c(i)) /= 3) error stop
   end do

   d = 'g'     ! the array is populated from character "g   " after padding
   do i = 1, 6
      print *, d(i), len(d(i))
      if (d(i) /= "g   ") error stop
      if (len(d(i)) /= 4) error stop
   end do

   e = "def"    ! the array is populated from character "de" after trimming
   do i = 1, 6
      print *, e(i), len(e(i))
      if (e(i) /= "de") error stop
      if (len(e(i)) /= 2) error stop
   end do



end program character_07


