! An internal procedure uses host variables that live in a common block,
! next to an ordinary host variable, with a procedure and a main program as
! the host
subroutine host_assoc_11_sub()
implicit none
integer :: cc, arr(3), k
common /host_assoc_11_blk/ cc, arr
cc = 5
arr = [1, 2, 3]
k = 10
call inner()
if (cc /= 6) error stop 1
if (any(arr /= [2, 3, 4])) error stop 2
if (k /= 11) error stop 3
contains
subroutine inner()
cc = cc + 1
arr = arr + 1
k = k + 1
end subroutine
end subroutine

program host_assoc_11
implicit none
integer :: cc, arr(3), k
common /host_assoc_11_blk/ cc, arr
call host_assoc_11_sub()
if (cc /= 6) error stop 4
if (any(arr /= [2, 3, 4])) error stop 5
k = 1
call inner()
if (cc /= 7) error stop 6
if (any(arr /= [4, 6, 8])) error stop 7
if (k /= 2) error stop 8
print *, cc, arr, k
contains
subroutine inner()
cc = cc + 1
arr = 2 * arr
k = k + 1
end subroutine
end program
