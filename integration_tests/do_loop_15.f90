! The programs of #12294: a zero-trip loop leaves its DO variable at the
! start, and a completed loop one step past the end.
program do_loop_15
   implicit none
   character(len=8) :: table(3)
   integer :: n, idx, i

   table = [character(len=8) :: "alpha", "beta", "gamma"]
   n = 0   ! the table is empty, so the search loop runs no iteration

   ! Find "beta": a zero-trip loop leaves idx == 1 == n + 1, so the guard
   ! below reports it as not found.
   do idx = 1, n
      if (trim(table(idx)) == "beta") exit
   end do

   if (idx > n) then
      print *, "not found (ok)"
   else
      print *, "found at idx=", idx, " value=", trim(table(idx))
      error stop "zero-trip search"
   end if

   do i = 1, 5
   end do
   if (i /= 6) error stop "expected i==6"
end program
