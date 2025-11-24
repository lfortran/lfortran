module arrays_100_mod
character :: arr(3) = ['a', 'b', 'c']
end module

program arrays_100
use arrays_100_mod
if (arr(1) /= 'a') error stop
if (arr(2) /= 'b') error stop
if (arr(3) /= 'c') error stop
print *, "All tests passed"
end program
