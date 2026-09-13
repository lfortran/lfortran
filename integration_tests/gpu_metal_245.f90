module gpu_metal_245_iface
! A device function called from a DO CONCURRENT is spliced into the loop
! body when it needs a run-time sized temporary, because Metal has
! neither variable-length arrays nor a heap inside a device function.
! For a submodule `module procedure` the call site names the parent
! module's interface declaration, whose body is empty, so the check for
! such temporaries used to see nothing, offload the loop anyway, and emit
! the device function verbatim -- run-time sized locals and all. The
! Metal shader then failed to compile at load time.
implicit none
interface
    pure module function submod_mv(mat, vec) result(r)
        implicit none
        real, intent(in) :: mat(:,:)
        real, intent(in) :: vec(:)
        real, allocatable :: r(:)
    end function
end interface
end module

submodule(gpu_metal_245_iface) gpu_metal_245_impl
implicit none
contains
    module procedure submod_mv
        real, allocatable :: tail(:)
        associate (ncols => size(mat, 2))
            allocate(tail(size(vec) - ncols))
            tail = 5.0
            allocate(r(size(mat, 1) + size(vec) - ncols))
            r = [matmul(mat, vec(1:ncols)), tail]
        end associate
    end procedure
end submodule

module gpu_metal_245_plain
! The same callee written as an ordinary module function, so the two
! spellings are covered side by side.
implicit none
contains
    pure function plain_mv(mat, vec) result(r)
        real, intent(in) :: mat(:,:)
        real, intent(in) :: vec(:)
        real, allocatable :: r(:)
        real, allocatable :: tail(:)
        associate (ncols => size(mat, 2))
            allocate(tail(size(vec) - ncols))
            tail = 5.0
            allocate(r(size(mat, 1) + size(vec) - ncols))
            r = [matmul(mat, vec(1:ncols)), tail]
        end associate
    end function
end module

module gpu_metal_245_drivers
use gpu_metal_245_iface
use gpu_metal_245_plain
implicit none
contains

    subroutine drive_submodule(op, v, out)
        real, intent(in) :: op(:,:)
        real, intent(in) :: v(:,:)
        real, intent(inout) :: out(:,:)
        integer :: j
        do concurrent (j = 1:size(v, 2))
            out(:,j) = submod_mv(op, v(:,j))
        end do
    end subroutine

    subroutine drive_plain(op, v, out)
        real, intent(in) :: op(:,:)
        real, intent(in) :: v(:,:)
        real, intent(inout) :: out(:,:)
        integer :: j
        do concurrent (j = 1:size(v, 2))
            out(:,j) = plain_mv(op, v(:,j))
        end do
    end subroutine

    ! A loop that has always been offloadable. It keeps this test from
    ! passing vacuously if GPU offload stops happening altogether.
    subroutine drive_fence(a, b)
        real, intent(in) :: a(:)
        real, intent(out) :: b(:)
        integer :: j
        do concurrent (j = 1:size(a))
            b(j) = 2.0 * a(j) + 1.0
        end do
    end subroutine

end module

program gpu_metal_245
use gpu_metal_245_drivers
implicit none
real :: op(1,2), v(4,2), out(3,2)
real :: a(5), b(5)
integer :: j

op = reshape([2.0, 3.0], [1,2])
v = 0.0
v(1:2,1) = [1.0, 2.0]
v(1:2,2) = [3.0, 4.0]

out = 0.0
call drive_submodule(op, v, out)
print *, out
if (abs(out(1,1) - 8.0) > 1.0e-5) error stop "submodule: matmul segment, column 1"
if (abs(out(1,2) - 18.0) > 1.0e-5) error stop "submodule: matmul segment, column 2"
do j = 1, 2
    if (abs(out(2,j) - 5.0) > 1.0e-5) error stop "submodule: tail element 2"
    if (abs(out(3,j) - 5.0) > 1.0e-5) error stop "submodule: tail element 3"
end do

out = 0.0
call drive_plain(op, v, out)
print *, out
if (abs(out(1,1) - 8.0) > 1.0e-5) error stop "plain: matmul segment, column 1"
if (abs(out(1,2) - 18.0) > 1.0e-5) error stop "plain: matmul segment, column 2"
do j = 1, 2
    if (abs(out(2,j) - 5.0) > 1.0e-5) error stop "plain: tail element 2"
    if (abs(out(3,j) - 5.0) > 1.0e-5) error stop "plain: tail element 3"
end do

do j = 1, 5
    a(j) = real(j)
end do
b = 0.0
call drive_fence(a, b)
print *, b
do j = 1, 5
    if (abs(b(j) - (2.0 * real(j) + 1.0)) > 1.0e-5) error stop "fence loop"
end do

print *, "ok"
end program
