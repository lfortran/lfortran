program read_93
   call s()
contains
   subroutine s()
      integer :: i
      open(86, file="/dev/null")
      if (i > 0) then
         read(86, *, end=20) i
20       print *, 'a'
      end if
      close(86)
   end subroutine
end program