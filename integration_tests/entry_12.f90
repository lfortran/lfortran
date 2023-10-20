subroutine dinvr(x)
integer :: x
go to 310
return

entry dstinv()
return

310 continue
print *, "x = ", x
if (x /= 12) error stop
end

program entry_09
call dinvr(12)
end program
