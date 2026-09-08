! A `do concurrent` whose body calls a function that builds its array result
! with an array constructor holding an array-valued *expression* --
! `0.5*(centers(1:n-1) + centers(2:n))` -- is offloaded.
!
! The constructor's temporary is sized `1 + size(<that expression>) + 1`, and
! an elementwise array expression records no shape in its own type. It has
! exactly the shape of its array operand, so the extent is resolved by walking
! down to that operand; without it the loop was declined with
! `workspace-extent-unresolvable` and left on the host.
!
! The callee's result variable is deliberately named `faces`, the same as the
! caller's array: the spliced-in body must not take a name that shadows it.
module gpu_metal_318_mod
implicit none
private
public :: face_values
contains
    pure function face_values(centers) result(faces)
        real, intent(in) :: centers(:)
        real :: faces(size(centers)+1)
        integer :: n
        n = size(centers)
        faces = [ centers(1), 0.5*(centers(1:n-1) + centers(2:n)), centers(n) ]
    end function
end module

program gpu_metal_318
use gpu_metal_318_mod, only : face_values
implicit none
integer, parameter :: nx = 4, ny = 3
real :: centers(nx, ny), faces(nx+1, ny), expected(nx+1, ny)
integer :: i, j

do j = 1, ny
    do i = 1, nx
        centers(i,j) = real(i + 10*j)
    end do
end do

do j = 1, ny
    expected(1,j) = centers(1,j)
    do i = 1, nx-1
        expected(i+1,j) = 0.5*(centers(i,j) + centers(i+1,j))
    end do
    expected(nx+1,j) = centers(nx,j)
end do

do concurrent (integer :: k = 1:ny)
    faces(:,k) = face_values(centers(:,k))
end do

do j = 1, ny
    do i = 1, nx+1
        if (abs(faces(i,j) - expected(i,j)) > 1.0e-5) error stop
    end do
end do

print *, faces(:,1)
print *, faces(:,ny)
end program
