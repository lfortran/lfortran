subroutine dzror()
implicit none
double precision zxlo
double precision xxlo
save

print *, 'xxlo = ', xxlo
if (abs(xxlo - 1.93D0) > 1d-10) error stop

entry dstzr(zxlo)
xxlo = zxlo
return
end subroutine

program entry_11
implicit none
double precision :: zxlo
zxlo = 1.93D0
call dstzr(zxlo)
call dzror()
end program
