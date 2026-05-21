program format_92
! Test G format descriptor trailing blanks when value is in F range
! Fortran standard: Gw.dEe in F mode outputs F(w-n).(d-k) + n blanks
! where n = e+2, k = floor(log10(|value|)) + 1
implicit none
character(len=14) :: s

! G14.5E3: n = 3+2 = 5, value 1.4457 in [1,10) so k=1, F9.4
write(s, '(G14.5E3)') 1.4457d0
if (s /= "   1.4457     ") error stop

! G14.5E3: value 0.12345 in [0.1,1) so k=0, F9.5
write(s, '(G14.5E3)') 0.12345d0
if (s /= "  0.12345     ") error stop

! G14.5E3: value 123.45 in [100,1000) so k=3, F9.2
write(s, '(G14.5E3)') 123.45d0
if (s /= "   123.45     ") error stop

! G14.5 (no explicit E): n = 4 (default), value 1.4457, k=1, F10.4
write(s, '(G14.5)') 1.4457d0
if (s /= "    1.4457    ") error stop

! G14.5E3: value outside F range uses E format (no trailing blanks)
write(s, '(G14.5E3)') 1.23d10
if (s /= "  0.12300E+011") error stop

print *, "All G format tests passed"
end program
