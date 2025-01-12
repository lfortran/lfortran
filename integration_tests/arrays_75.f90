program arrays_75
integer :: score(4) = [-1,1,3,2]

print * , [score, 1] > 0
if (([score,1] > 0) /= [.false. , .true. , .true. , .true. , .true.]) error stop

print * , 0 < [score , -1]
if ((0 < [score , -1]) /= [.false. , .true. , .true. , .true. , .false.]) error stop

print * , [score , 1] <= [score , -1]
if (([score , 1] <= [score , -1]) /= [.true. , .true. , .true. , .true. , .false.]) error stop
end program