program intrinsics_412
integer :: ii
call update(ii)
print *, ii
if (ii /= 1) error stop
contains
subroutine update(return_value)
integer, intent(out) :: return_value
character(len=:),allocatable :: long

character(len=:),allocatable,save :: shorts(:)

allocate(character(len=3) :: shorts(3))
shorts = ['abc', 'def', 'ghi']
long = 'banana'
return_value=maxloc([0,merge(1, 0, shorts == long)],dim=1)

end subroutine update
end program
