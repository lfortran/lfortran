integer, allocatable :: a(:)
integer, pointer :: p

allocate(a(10))
deallocate(a)

open(unit=10, file="test.txt", status="old")
flush(10)
close(unit=10)

nullify(p)

exit
cycle
return
continue

print *, exit
print *, cycle
print *, return
print *, continue

call exit
call cycle
call return
call continue

contains

subroutine sub
   integer :: i

   do i = 1, 10
      print *, i
      if (i == 5) exit
      if (i == 7) cycle
      if (i == 9) return
      if (i == 2) continue
   end do

end subroutine

end program
