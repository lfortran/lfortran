! An ASSOCIATE name bound to an ALLOCATABLE COMPONENT of a derived type
! is typed as a pointer to an array, not as an allocatable array. When
! such an associate name is passed as a `do concurrent` kernel argument,
! the GPU argument marshalling must treat it as a descriptor-backed
! array (data pointer + runtime extents), exactly like an allocatable.
module gpu_metal_227_mod
implicit none

type :: field_t
    real, allocatable :: line_(:)
    real, allocatable :: cube_(:,:,:)
end type

end module

program gpu_metal_227
use gpu_metal_227_mod
implicit none

type(field_t) :: f
real :: out1(4)
real :: out3(2,3,4)
integer :: i, j, k
integer :: p, q, r

allocate(f%line_(4))
do i = 1, 4
    f%line_(i) = real(i)
end do

allocate(f%cube_(2,3,4))
do k = 1, 4
    do j = 1, 3
        do i = 1, 2
            f%cube_(i,j,k) = real(i + 10*j + 100*k)
        end do
    end do
end do

out1 = 0.0
associate(v => f%line_)
    do concurrent(p = 1:4)
        out1(p) = 2.0*v(p)
    end do
end associate
do i = 1, 4
    if (abs(out1(i) - 2.0*real(i)) > 1.0e-5) error stop "wrong rank-1 value"
end do
print *, out1

out3 = 0.0
associate(c => f%cube_)
    do concurrent(q = 1:4, r = 1:3)
        out3(1,r,q) = 3.0*c(1,r,q)
        out3(2,r,q) = 3.0*c(2,r,q)
    end do
end associate
do k = 1, 4
    do j = 1, 3
        do i = 1, 2
            if (abs(out3(i,j,k) - 3.0*real(i + 10*j + 100*k)) > 1.0e-2) then
                error stop "wrong rank-3 value"
            end if
        end do
    end do
end do
print *, out3(1,1,1), out3(2,3,4)

deallocate(f%line_)
deallocate(f%cube_)
end program
