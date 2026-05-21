program entry_19
! Test that calling a subroutine with an ENTRY point whose dummy-argument
! list includes an array (different from the main subroutine's arguments)
! does not cause a spurious "unallocated" runtime error.
implicit none
integer :: ix, iy, ic, id, i2d(2,2)

ix = 3
call sub1(ix, iy)
if (iy /= 6) error stop

ic = 5
i2d = 0
call ent1(ic, i2d, id)
if (id /= 0) error stop

print *, "PASS"
end program

subroutine sub1(ia, ib)
implicit none
integer, intent(in) :: ia
integer, intent(out) :: ib
integer :: ic, id, i2d(2,2)
ib = ia + 3
return
entry ent1(ic, i2d, id)
id = 0
return
end subroutine
