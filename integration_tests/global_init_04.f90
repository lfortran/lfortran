! Array pointer declaration initializers.
!
! Only labelled `llvm`: gfortran 16.1.0 leaves such a pointer associated with
! nothing at all (`associated` is false and `size` is 1), while flang agrees
! with the results checked below.
!
! The same declaration in a procedure is not checked here: a `save` array
! pointer of a procedure loses its association after the first call whichever
! way it is associated, which is a separate bug of its own.
module global_init_04_m
implicit none

integer, save, target :: arr(3) = [1, 2, 3]
integer, pointer :: p_arr(:) => arr

end module

program global_init_04
use global_init_04_m
implicit none

if (.not. associated(p_arr, arr)) error stop 1
if (size(p_arr) /= 3) error stop 2
if (lbound(p_arr, 1) /= 1) error stop 3
if (ubound(p_arr, 1) /= 3) error stop 4
if (p_arr(2) /= 2) error stop 5

! The pointer really is an alias of its target, not a copy of it.
p_arr(2) = 20
if (arr(2) /= 20) error stop 6
arr(3) = 30
if (p_arr(3) /= 30) error stop 7

print *, "ok"
end program
