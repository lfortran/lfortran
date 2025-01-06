! https://github.com/lfortran/lfortran/issues/5872
program main
   type tt
   end type tt
   ! Class declaration must be dummy, allocatable, or pointer
   class(tt) :: inst_tt
 end program main
