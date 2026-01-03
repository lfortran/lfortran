subroutine sub()
integer :: x
x = 12
go to 50
go to 60
return

50 print *, "here at 50, x = ", x
if (x /= 12) error stop
x = 13
go to 60
60 print *, "here at 60, x = ", x
if (x /= 13) error stop
x = 19
go to 70
return

entry entry_sub1()
return

70 print *, "here at 70, x = ", x
if (x /= 19) error stop
return
end subroutine

program entry_13
    call sub()
end program
