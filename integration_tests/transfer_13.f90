program transfer_13
implicit none

character(len=3) :: result
result = transfer(['x','y','z'], 'abc')
if (result /= 'xyz') error stop

result = transfer(['a','b','c'], 'XYZ')
if (result /= 'abc') error stop

print *, "All tests passed."
end program
