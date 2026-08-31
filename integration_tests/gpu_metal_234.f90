module gpu_metal_234_mod
implicit none

contains

    ! Read through an assumed-shape dummy inside a `do concurrent`.
    subroutine copy_dc(vec, r, n)
        real, intent(in) :: vec(:)
        integer, intent(in) :: n
        real, intent(out) :: r(n)
        integer :: j
        do concurrent (j = 1:n)
            r(j) = vec(j)
        end do
    end subroutine

    ! Write through an assumed-shape dummy inside a `do concurrent`.
    subroutine fill_dc(vec, n)
        real, intent(out) :: vec(:)
        integer, intent(in) :: n
        integer :: j
        do concurrent (j = 1:n)
            vec(j) = 100.0 + real(j)
        end do
    end subroutine

    ! Fence: the same access from a plain `do` loop is never offloaded.
    subroutine copy_plain(vec, r, n)
        real, intent(in) :: vec(:)
        integer, intent(in) :: n
        real, intent(out) :: r(n)
        integer :: j
        do j = 1, n
            r(j) = vec(j)
        end do
    end subroutine

    ! An array-valued function taking the section as its argument.
    function scaled(vec) result(res)
        real, intent(in) :: vec(:)
        real :: res(3)
        integer :: j
        do concurrent (j = 1:3)
            res(j) = 2.0*vec(j)
        end do
    end function

end module

program gpu_metal_234
use gpu_metal_234_mod
implicit none
real, target :: a(6,5)
real :: b(6,5)
real, target :: c(4,3,2)
real, pointer :: p(:)
real :: r(3)
integer :: i, j, k

do j = 1, 5
    do i = 1, 6
        a(i,j) = real(i) + 10.0*real(j)
    end do
end do
b = a
do k = 1, 2
    do j = 1, 3
        do i = 1, 4
            c(i,j,k) = real(i) + 10.0*real(j) + 100.0*real(k)
        end do
    end do
end do

! 1. Read through an assumed-shape dummy bound to a strided section.
r = 0.0
call copy_dc(a(3,:), r, 3)
print *, "strided dummy read:", r
if (any(abs(r - [13.0, 23.0, 33.0]) > 1.0e-5)) error stop "strided dummy read"

! 2. Rank-3 source, section a(2,:,2).
r = 0.0
call copy_dc(c(2,:,2), r, 3)
print *, "rank-3 section read:", r
if (any(abs(r - [212.0, 222.0, 232.0]) > 1.0e-5)) error stop "rank-3 section read"

! 3. Pointer associated with a strided section, read inside a `do concurrent`.
p => a(3,:)
r = 0.0
do concurrent (j = 1:3)
    r(j) = p(j)
end do
print *, "pointer to section:", r
if (any(abs(r - [13.0, 23.0, 33.0]) > 1.0e-5)) error stop "pointer to section"

! 4. The same pointer passed on as an assumed-shape dummy.
r = 0.0
call copy_dc(p, r, 3)
print *, "pointer as dummy:", r
if (any(abs(r - [13.0, 23.0, 33.0]) > 1.0e-5)) error stop "pointer as dummy"

! 5. A strided section as the argument of an array-valued function.
r = 0.0
r = scaled(a(3,1:3))
print *, "section into function:", r
if (any(abs(r - [26.0, 46.0, 66.0]) > 1.0e-5)) error stop "section into function"

! 6. Fence: contiguous section still reads correctly.
r = 0.0
call copy_dc(a(1:3,1), r, 3)
print *, "contiguous section:", r
if (any(abs(r - [11.0, 12.0, 13.0]) > 1.0e-5)) error stop "contiguous section"

! 7. Fence: a plain `do` over the same strided section.
r = 0.0
call copy_plain(a(3,:), r, 3)
print *, "plain do:", r
if (any(abs(r - [13.0, 23.0, 33.0]) > 1.0e-5)) error stop "plain do"

! 8. Fence: indexing the section directly in the caller.
r = 0.0
do concurrent (j = 1:3)
    r(j) = a(3,j)
end do
print *, "caller-side index:", r
if (any(abs(r - [13.0, 23.0, 33.0]) > 1.0e-5)) error stop "caller-side index"

! 9. Write through an assumed-shape dummy bound to a strided section.
call fill_dc(b(3,1:3), 3)
print *, "strided dummy write:", b(3,1), b(3,2), b(3,3)
if (abs(b(3,1) - 101.0) > 1.0e-5) error stop "strided dummy write 1"
if (abs(b(3,2) - 102.0) > 1.0e-5) error stop "strided dummy write 2"
if (abs(b(3,3) - 103.0) > 1.0e-5) error stop "strided dummy write 3"
! Neighbouring elements inside the strided span must be untouched.
if (abs(b(4,1) - 14.0) > 1.0e-5) error stop "strided dummy write neighbour 1"
if (abs(b(2,2) - 22.0) > 1.0e-5) error stop "strided dummy write neighbour 2"
if (abs(b(6,5) - 56.0) > 1.0e-5) error stop "strided dummy write neighbour 3"

print *, "OK"

end program
