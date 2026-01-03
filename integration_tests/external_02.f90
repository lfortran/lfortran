subroutine cdfpoi()
   implicit none
   external cumpoi
   return
end subroutine

program external_02
   call cdfpoi()
end program
