! A structure component used to be identified by its member symbol alone
! when deciding whether the two sides of an array assignment inside a
! `do concurrent` overlap. `a%values_` and `b%values_` therefore looked
! like the same storage even for two completely different objects, and
! every assignment between them was reported as aliasing. With a
! run-time sized component the temporary that implies cannot be built on
! the GPU, so the whole loop was silently left on the host.
!
! A designator is now identified by the pair (root variable, member
! path), so two designators overlap only when the root variable is the
! same and the whole member path matches. Genuine overlap still gets its
! temporary.
!
! `--gpu=metal --show-gpu-kernel-source` reports 4 kernels for this
! program, one per loop below; before the fix it reported 3, the first
! loop having been declined. The decline was silent and the results were
! correct either way, so the kernel count is what the fix is about.
program gpu_metal_238
implicit none

type :: inner_t
    real, allocatable :: values_(:,:)
end type inner_t

type :: fixed_t
    real :: f_(4,3)
end type fixed_t

type :: outer_t
    type(fixed_t) :: a
    type(fixed_t) :: b
end type outer_t

type(inner_t) :: p, q
type(outer_t) :: u, v, w
type(fixed_t) :: z
integer :: i, j
integer, parameter :: n = 4, m = 3

allocate(p%values_(n,m), q%values_(n,m))

do j = 1, m
    do i = 1, n
        p%values_(i,j) = 0.0
        q%values_(i,j) = real(i + 10*j)
        u%a%f_(i,j) = 0.0
        v%a%f_(i,j) = real(i + 10*j)
        w%a%f_(i,j) = 0.0
        w%b%f_(i,j) = real(i + 10*j)
        z%f_(i,j) = real(i + 10*j)
    end do
end do

! Two distinct objects that only share a component name: no overlap, so
! this must be offloaded rather than declined for a temporary.
do concurrent (j = 1:m)
    p%values_(:,j) = q%values_(:,j) * 2.0
end do

! Distinct roots reached through a nested component path.
do concurrent (j = 1:m)
    u%a%f_(:,j) = v%a%f_(:,j) + 1.0
end do

! Same root, different member paths: `w%a` and `w%b` cannot overlap.
do concurrent (j = 1:m)
    w%a%f_(:,j) = w%b%f_(:,j) * 3.0
end do

! Same object on both sides through the same member path, with a
! genuinely overlapping reversed section: this still needs a temporary,
! and an ascending element copy without one would give 4 3 2 4 ... .
do concurrent (j = 1:m)
    z%f_(:,j) = z%f_(4:1:-1,j)
end do

do j = 1, m
    do i = 1, n
        if (abs(p%values_(i,j) - 2.0*real(i + 10*j)) > 1.0e-6) then
            error stop "distinct objects, shared component name"
        end if
        if (abs(u%a%f_(i,j) - (real(i + 10*j) + 1.0)) > 1.0e-6) then
            error stop "distinct roots, nested member path"
        end if
        if (abs(w%a%f_(i,j) - 3.0*real(i + 10*j)) > 1.0e-6) then
            error stop "same root, different member paths"
        end if
        if (abs(z%f_(i,j) - real(5 - i + 10*j)) > 1.0e-6) then
            error stop "same member path, real overlap"
        end if
    end do
end do

print *, "ok"
end program gpu_metal_238
