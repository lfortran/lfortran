program format_11
implicit none
logical :: b
print *, 'test writing a logical'
print *
b = .false.
print *, 'formatted write'
write (*, 700) .true.
write (*, 700) b
print *, 'using print'
print *, .true.
print *, b
stop
700 format (l7)
end program
