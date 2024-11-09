subroutine foo(ir, rr, lr)
integer, intent(in) :: ir
real, intent(in)    :: rr
logical, intent(in) :: lr
integer, save :: ic = 1
real, save    :: rc = 1
logical, save :: lc = .true.
ic = ic + 1
rc = rc + 1
lc = .not. lc
print *, 'FOO: ', ic, ir, rc, rr, lc, lr
if (ic /= ir) error stop
if (rc /= rr) error stop
if (lc .neqv. lr) error stop
end subroutine

program save_09
integer i
logical :: lr = .false.
do i = 2, 4
    call foo(i, real(i), lr)
    lr = .not. lr
end do
end program
