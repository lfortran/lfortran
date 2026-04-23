program intrinsics_454
implicit none

! Test all() with both array section and whole array in the same scope.
! This triggered a type mismatch when the Pointer-typed section helper
! was incorrectly reused for the plain array call.

logical :: x(3)
logical :: y(2, 2)

x = .true.

! array section (Pointer) then whole array
if (.not. (all(x(1:2)) .and. all(x))) error stop
print *, "test 1 passed"

! whole array then array section
if (.not. (all(x) .and. all(x(2:3)))) error stop
print *, "test 2 passed"

! any() shares the same instantiation path
if (.not. (any(x(1:1)) .and. any(x))) error stop
print *, "test 3 passed"

! multi-dimensional
y = .true.
if (.not. (all(y(1:1, :)) .and. all(y))) error stop
print *, "test 4 passed"

x(2) = .false.
if (all(x)) error stop
if (.not. all(x(1:1))) error stop
print *, "test 5 passed"

end program
