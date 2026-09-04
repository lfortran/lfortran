program duplicate_end_if_label1
implicit none
integer :: i
i = 0
86 continue
i = i + 1
if (i < 3) then
   go to 86
86 end if
print *, i
end program
