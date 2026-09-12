module elemental_25_mod
implicit none
contains
    elemental function add(a, b) result(r)
        integer, intent(in) :: a, b
        integer :: r
        r = a + b
    end function

    elemental function neg(a) result(r)
        integer, intent(in) :: a
        integer :: r
        r = -a
    end function
end module

program elemental_25
use elemental_25_mod
implicit none
integer :: a3(2,3,4), mm(2,3), v(3), w(4)
integer :: i, j, k

mm = reshape([1, 2, 3, 4, 5, 6], [2, 3])
do i = 1, 2
    do j = 1, 3
        do k = 1, 4
            a3(i,j,k) = i * 100 + j * 10 + k
        end do
    end do
end do

! A scalar subscript in the first dimension reduces the rank, so the result
! must follow the remaining dimension, not the collapsed one
v = add(mm(1,:), 1)
if (v(1) /= 2) error stop
if (v(2) /= 4) error stop
if (v(3) /= 6) error stop
print *, v

v = add(mm(2,:), 1)
if (v(1) /= 3) error stop
if (v(3) /= 7) error stop
print *, v

v = neg(mm(1,:))
if (v(1) /= -1) error stop
if (v(3) /= -5) error stop
print *, v

! A scalar subscript in the last dimension still works
if (size(add(mm(:,1), 1)) /= 2) error stop
if (any(add(mm(:,1), 1) /= [2, 3])) error stop
print *, add(mm(:,1), 1)

! Rank-3 sections with the scalar subscript in each position
if (size(add(a3(1,:,1), 1)) /= 3) error stop
if (any(add(a3(1,:,1), 1) /= [112, 122, 132])) error stop
print *, add(a3(1,:,1), 1)

if (size(add(a3(:,1,1), 1)) /= 2) error stop
if (any(add(a3(:,1,1), 1) /= [112, 212])) error stop
print *, add(a3(:,1,1), 1)

w = add(a3(1,1,:), 1)
if (w(1) /= 112) error stop
if (w(4) /= 115) error stop
print *, w

! Rank-2 results from a rank-3 array
if (any(shape(add(a3(1,:,:), 0)) /= [3, 4])) error stop
if (any(shape(add(a3(:,1,:), 0)) /= [2, 4])) error stop
if (any(shape(add(a3(:,:,1), 0)) /= [2, 3])) error stop
print *, add(a3(1,:,:), 0)
print *, add(a3(:,1,:), 0)
print *, add(a3(:,:,1), 0)

! Strided and reversed sections after a scalar subscript
if (any(add(mm(1,1:3:2), 1) /= [2, 6])) error stop
print *, add(mm(1,1:3:2), 1)

if (any(add(mm(1,3:1:-1), 1) /= [6, 4, 2])) error stop
print *, add(mm(1,3:1:-1), 1)

if (any(add(a3(1,2:3,2), 0) /= [122, 132])) error stop
print *, add(a3(1,2:3,2), 0)

! Nested elemental calls and array-valued second argument
if (any(add(add(mm(1,:), 1), 1) /= [3, 5, 7])) error stop
print *, add(add(mm(1,:), 1), 1)

if (any(add(mm(1,:), v) /= [0, 0, 0])) error stop
print *, add(mm(1,:), v)

! Reductions over the section result
if (sum(add(mm(1,:), 1)) /= 12) error stop
if (maxval(add(a3(1,:,1), 0)) /= 131) error stop
print *, sum(add(mm(1,:), 1)), maxval(add(a3(1,:,1), 0))

! A whole array argument is unaffected
if (any(add(mm, 1) /= reshape([2, 3, 4, 5, 6, 7], [2, 3]))) error stop
print *, add(mm, 1)

end program elemental_25
