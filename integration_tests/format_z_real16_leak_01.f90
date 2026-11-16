program format_z_real16_leak_01
! Z editing of real(16) values must not leak runtime buffers (#17861)
implicit none
real(16) :: x(3)
character(len=50) :: s

x = [1.5_16, -2.0_16, 0.0_16]

write(s, '(z32.32)') x(1)
if (trim(s) /= "3FFF8000000000000000000000000000") error stop 1
write(s, '(z32.32)') x(2)
if (trim(s) /= "C0000000000000000000000000000000") error stop 2
write(s, '(a,z0,a)') '[', x(3), ']'
if (trim(s) /= "[0]") error stop 3
write(s, '(z0)') x(1)
if (trim(s) /= "3FFF8000000000000000000000000000") error stop 4
write(s, '(a,z36.34,a)') '[', x(1), ']'
if (trim(s) /= "[  003FFF8000000000000000000000000000]") error stop 5
write(s, '(a,z40,a)') '[', x(2), ']'
if (trim(s) /= "[        C0000000000000000000000000000000]") error stop 6
write(s, '(a,z4,a)') '[', x(1), ']'
if (trim(s) /= "[****]") error stop 7
write(s, '(a,z4.2,a)') '[', x(3), ']'
if (trim(s) /= "[  00]") error stop 8
write(s, '(a,z3.3,a)') '[', x(3), ']'
if (trim(s) /= "[000]") error stop 9
write(s, '(a,3z1,a)') '[', x, ']'
if (trim(s) /= "[**0]") error stop 10

print '(z32.32)', x(1)
print '(z0)', x(2)
print '(3z4)', x
end program
