subroutine a(w)
double precision :: w(5)
integer icon
icon = 2
call b(w(icon))
return
end

subroutine b(con)
double precision :: con(*)
con(4) = -4.83D0
con(3) = 3.14D0
return
end

program legacy_array_sections_01
double precision :: w(5)
w = 1.39D0
call a(w)
print *, w
if (abs(w(5) - (-4.83D0)) > 1d-8) error stop
if (abs(w(4) - 3.14D0) > 1d-10) error stop
end program
