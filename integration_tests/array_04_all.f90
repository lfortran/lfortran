program array_04_all
implicit none
logical l(4)
l = all([.true., .true., .true., .true.])
if (.not. all(l)) error stop
if (.not. all(l, dim = 0)) error stop
print *, l
end program
