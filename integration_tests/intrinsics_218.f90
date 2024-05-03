program main
implicit none

integer, parameter :: new_len = len(new_line('a'))
print *, new_len
if (new_len /= 1) error stop
print*, "Hello, World!", new_line('a')
end
