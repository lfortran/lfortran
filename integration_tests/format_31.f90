program format_31
implicit none
   call printit(3*1365+1)
   call printit(3*37+1)
contains
subroutine printit(ii)
integer,intent(in) :: ii
integer            :: nz, kz
character(len=*),parameter :: show32 = '(1x,"value=",i4,", value(bits)=",b32.32,1x,", trailz=",i3)'
   print '(a)', repeat('-',80)
   print  show32, ii, ii, trailz(ii)
   call trimz(ii,nz,kz)
   print  show32,  kz, kz, trailz(kz)
   print  '(b32.32)', 4096
   print  '(b32.16)', 4096
   print  '(b32)', 4096
   print  '(b32.8)', 4096
end subroutine printit
subroutine trimz(v,nz,kz)
integer,intent(in)  :: v
integer,intent(out) :: nz
integer,intent(out) :: kz
  nz=trailz(v)
  kz=shiftr(v,nz)
end subroutine trimz
end program format_31