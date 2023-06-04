subroutine sub1()
integer illin, ntrep
common illin, ntrep
if (illin /= 1) error stop
if (ntrep /= 2) error stop
print *, illin, ntrep
end

program common_09
integer illin , ntrep
common illin, ntrep
illin = 1
ntrep = 2
call sub1()
end
