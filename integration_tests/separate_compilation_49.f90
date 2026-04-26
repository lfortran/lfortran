program separate_compilation_49
   implicit none
   interface
      subroutine separate_compilation_49_sub()
      end subroutine
   end interface
   ! A format with an `Nx` descriptor (N >= 4) used to leave non-zero garbage
   ! in the heap region that was later returned by malloc to back the data
   ! pointer of the module-level allocatable array `d`. Combined with the
   ! rank field of `d`'s descriptor never being initialized at allocation
   ! time (it lives in .bss with separate compilation), only the first
   ! element's allocatable component pointer was nullified, so `d(2)%a`
   ! appeared "already allocated".
   write(*,"(4x)")
   call separate_compilation_49_sub()
end program
