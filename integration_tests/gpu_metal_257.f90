! A kernel argument that the offload pass reaches only through an
! allocatable component of a struct is decomposed: `s` is dropped and the
! component `s%c_` is passed in its place, so the actual argument of the
! launch is a `StructInstanceMember` rather than a plain variable.
!
! The Metal code generator declares three extra device buffers -- data,
! offsets and sizes -- for every allocatable array component of a kernel
! argument that is an array of derived type, and the buffer accounting
! counts them.  The host, though, decided whether to bind them from the
! *actual* argument and required it to be a plain variable, so for a
! decomposed argument it bound none of the three.  Every buffer index
! after the struct then shifted by three: the scalar-argument struct was
! written to buffer 2 while the shader read it from buffer 5, which is
! uninitialized memory.  The kernel read garbage for every scalar it has,
! its loop bound included, and the program printed wrong values with no
! diagnostic of any kind.
!
! The three emitters now enumerate those components through one shared
! function, and the host derives the layout from the kernel parameter --
! which is what the shader signature is written from -- instead of from
! the actual.
!
! `run_decomposed` and `run_plain` offload, so this program emits exactly
! 2 kernels.  `run_member_sized` sizes a workspace from
! `size(s%c_(1)%m_)`, an extent the host-side resolver cannot reproduce
! from the kernel arguments, and is still declined with
! `status=host reason=workspace-extent-unresolvable`; it is here to check
! that declining leaves the right answer behind.
module gpu_metal_257_mod
implicit none

type :: leaf_t
    real, allocatable :: m_(:)
end type

type :: nest_t
    type(leaf_t), allocatable :: c_(:)
end type

contains

    pure function scale2(v) result(res)
    real, intent(in) :: v(:)
    real, allocatable :: res(:)
    integer :: k
    allocate(res(size(v)))
    do k = 1, size(v)
        res(k) = 2.0 * v(k)
    end do
    end function

    ! `s` itself is never touched, only `s%c_`, so the offload pass drops
    ! `s` and passes the component.  Every iteration reads a different
    ! element, so a kernel that mixes threads up cannot pass the checks.
    subroutine run_decomposed(o, s, nn)
    real, intent(out) :: o(:,:)
    type(nest_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = scale2(s%c_(jj)%m_)
    end do
    end subroutine

    ! The same shape with the struct array passed directly, which has
    ! always taken the host's plain-variable path.  It is the fence: the
    ! fix must not disturb it.
    subroutine run_plain(o, c, nn)
    real, intent(out) :: o(:,:)
    type(leaf_t), intent(in) :: c(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = scale2(c(jj)%m_)
    end do
    end subroutine

    ! A per-thread workspace whose extent is `size(s%c_(1)%m_)`.  The
    ! host cannot work that out from the kernel arguments, so the loop
    ! stays on the host and has to produce the right answer there.
    subroutine run_member_sized(o, s, nn)
    real, intent(out) :: o(:)
    type(nest_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            real :: acc
            integer :: p
            allocate(w(size(s%c_(1)%m_)))
            do p = 1, size(s%c_(1)%m_)
                w(p) = real(10*jj + p)
            end do
            acc = 0.0
            do p = 1, size(s%c_(1)%m_)
                acc = acc + w(p)
            end do
            o(jj) = acc
        end block
    end do
    end subroutine

end module

program gpu_metal_257
use gpu_metal_257_mod
implicit none
type(nest_t) :: s
type(leaf_t), allocatable :: c(:)
real :: o(3,4), op(3,4), ow(4)
real :: expected
integer :: i, j

allocate(s%c_(4))
allocate(c(4))
do j = 1, 4
    allocate(s%c_(j)%m_(3))
    allocate(c(j)%m_(3))
    do i = 1, 3
        s%c_(j)%m_(i) = real(10*j + i)
        c(j)%m_(i) = real(10*j + i)
    end do
end do

o = -1.0
call run_decomposed(o, s, 4)
print *, o
do j = 1, 4
    do i = 1, 3
        if (abs(o(i,j) - 2.0 * real(10*j + i)) > 1.0e-5) then
            error stop "decomposed struct argument"
        end if
    end do
end do

op = -1.0
call run_plain(op, c, 4)
print *, op
do j = 1, 4
    do i = 1, 3
        if (abs(op(i,j) - 2.0 * real(10*j + i)) > 1.0e-5) then
            error stop "plain struct argument"
        end if
    end do
end do

ow = -1.0
call run_member_sized(ow, s, 4)
print *, ow
do j = 1, 4
    expected = 0.0
    do i = 1, 3
        expected = expected + real(10*j + i)
    end do
    if (abs(ow(j) - expected) > 1.0e-5) error stop "member sized workspace"
end do

print *, "ok"
end program
