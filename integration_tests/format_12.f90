program format_12
implicit none
print *, 'testing output using formatting'
write (*, 700) 1, 1.23, (7., 8.), 'Hello', .true.
write (*, 701)
write (*, 702)
700 format (i5, e12.4e3, 2f8.2, 1x, a3, l7)
701 format ('12345678901234567890123456789012345678901234567890')
702 format ('         1         2         3         4         5')
write(*, '(i5, e12.4e3, 2f8.2, 1x, a3, l7)') 1, 1.23, (7.,8.), 'Hello', .true.
print *, 'end output formatting test'
print *
end program
