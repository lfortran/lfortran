program array_04_pack
implicit none
integer :: dims(2)
integer, dimension(4) :: perm, dim_range = [1, 2, 3, 4]

dims = [2, 3]

! pack() with a runtime-dependent mask: the result size is unknown at compile time.
! The array constructor [dims, pack(...)] should have size 4, matching perm.
perm = [dims, pack(dim_range, dim_range /= dims(1) .and. dim_range /= dims(2))]

if (perm(1) /= 2) error stop
if (perm(2) /= 3) error stop
if (perm(3) /= 1) error stop
if (perm(4) /= 4) error stop

end program
