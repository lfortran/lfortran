program forall_02
integer :: arr(5)
forall (i = 1:5)
    arr(i) = i * 2
end forall
print *, arr
if (any(arr /= [2, 4, 6, 8, 10])) error stop
end program
