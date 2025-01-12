program arrays_75
integer :: score(4) = [-1,1,3,2]

print * , [score, 1] > 0
if (([score,1] > 0) /= [.false. , .true. , .true. , .true. , .true.]) error stop
end program