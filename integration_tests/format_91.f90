program format_91
implicit none
character(20) :: buf

! G format in F-mode must pad trailing blanks within its own field
write(buf, '(G10.2, I10)') 1.0d0, 42
! G10.2 for 1.0d0 in F-mode: F6.0 + 4 trailing blanks = "   1.0    "
! I10 for 42: "        42"
if (buf /= "   1.0            42") error stop

! Test with a different value in F-mode range
write(buf, '(G10.3, I10)') 3.14d0, 99
! G10.3 for 3.14: F6.3-like + 4 trailing blanks
if (buf /= "  3.14            99") error stop

! Test value that uses E-mode (outside F-mode range): no trailing blank padding
write(buf, '(G10.2, I10)') 1.0d5, 7
! 1.0d5 is >= 10**2, so uses E-mode: full width 10
if (buf /= "  0.10E+06         7") error stop

print *, "OK"
end program
