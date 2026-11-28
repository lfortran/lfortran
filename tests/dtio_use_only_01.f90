module m
 type t
  integer :: n = 0
 end type
 interface write(formatted)
  module procedure wft
 end interface
contains
 subroutine wft(d,u,io,v,ios,msg)
  class(t), intent(in) :: d
  integer, intent(in) :: u, v(:)
  character(*), intent(in) :: io
  integer, intent(out) :: ios; character(*), intent(inout) :: msg
  write(u,*) 'wft called'
 end
end
program p
 use m, only: t
 print *, t(1)
end
