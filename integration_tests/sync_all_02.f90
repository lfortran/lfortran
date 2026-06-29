program sync_all_02
implicit none
integer :: istat
character(len=64) :: emsg
istat = 0
emsg = ""
sync all (stat=istat, errmsg=emsg)
if (istat /= 0) error stop
print *, "ok"
end program
