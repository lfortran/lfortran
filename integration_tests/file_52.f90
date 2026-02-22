program file_52
   implicit none

   character(30) :: s1, s2, s3
   integer :: i
   logical :: l
   real :: r
   integer :: iounit

   open (newunit=iounit, file='ld_strings.dat', form='formatted', status='replace')

   write(iounit,'(A)') "'ABCDEF'"
   write(iounit,'(A)') "'AB' 'ABCD' 'ABCDEF'"
   write(iounit,'(A)') "-5,'2468',T,15.0"
   write(iounit,'(A)') "'CAN''T, AND/OR   WON''T'"

   close(iounit)

   open (newunit=iounit, file='ld_strings.dat', form='formatted', status='old')

   ! Test 1
   read (iounit, *) s1
   if (s1 /= 'ABCDEF') then
      error stop "Test 1 failed: s1 /= 'ABCDEF'"
   end if

   ! Test 2
   read (iounit, *) s1, s2, s3
   if (s1 /= 'AB' .or. s2 /= 'ABCD' .or. s3 /= 'ABCDEF') then
      error stop "Test 2 failed: s1, s2, s3 mismatch"
   end if

   ! Test 3
   read (iounit, *) i, s1, l, r
   if (i /= -5) then
      error stop "Test 3 failed: i /= -5"
   end if
   if (s1 /= '2468') then
      error stop "Test 3 failed: s1 /= '2468'"
   end if
   if (.not. l) then
      error stop "Test 3 failed: l is not true"
   end if
   if (r /= 15.0) then
      error stop "Test 3 failed: r /= 15.0"
   end if

   ! Test 4
   read (iounit, *) s1
   if (s1 /= "CAN'T, AND/OR   WON'T") then
      error stop "Test 4 failed: s1 mismatch"
   end if

   close (iounit)

end program
