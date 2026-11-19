module realloc_lhs_hoist_15_m
implicit none
type :: box_t
    real(4), dimension(:), allocatable :: v
contains
    procedure :: __libasr_intent_out_reset => __libasr_intent_out_reset_box_t
end type box_t

contains

subroutine component(n, s)
    integer(4) :: __libasr_created__result_metadata_
    integer(4), intent(in) :: n
    integer(4) :: __libasr_extent_a_1 = Max(0, n)
    integer(4) :: __libasr_index_0_
    real(4), dimension(__libasr_extent_a_1) :: a
    type(box_t) :: b
    integer(4) :: i
    real(4), intent(out) :: s
    do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_) = real(1, kind=4)
    end do
    s = real(0, kind=4)
    reallocate(b%v(size(a)))
    do i = 1, 10
        a(1) = _lcompilers_real_i32(i)
        __libasr_created__result_metadata_ = size(a)
        call f3(a, b%v)
        s = s + b%v(1)
    end do
    if (size(b%v) /= n .or. lbound(b%v, 1) /= 1) then
        error stop
    end if
end subroutine component

pure subroutine f3(a, r)
    real(4), dimension(:), intent(in) :: a
    integer(4) :: __libasr_extent_r_1 = size(a)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(__libasr_extent_r_1), intent(out) :: r
    ! CheckBounds LHS = r, RHS = a, 
    __libasr_index_0_1 = lbound(a, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = real(2, kind=4)*a(__libasr_index_0_1) + real(1, kind=4)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine f3

subroutine reallocated(n, s)
    type :: __lcompilers_frame_reallocated
        real(4), dimension(:), allocatable, reference :: c
    end type __lcompilers_frame_reallocated
    type(__lcompilers_frame_reallocated) :: __lcompilers_frame
    integer(4) :: __libasr_created__result_metadata_
    integer(4), intent(in) :: n
    integer(4) :: __libasr_extent_a_1 = Max(0, n)
    integer(4) :: __libasr_index_0_
    real(4), dimension(__libasr_extent_a_1) :: a
    real(4), dimension(:), allocatable :: c
    integer(4) :: i
    real(4), intent(out) :: s
    bind_reference(__lcompilers_frame%c, c)
    do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_) = real(1, kind=4)
    end do
    s = real(0, kind=4)
    do i = 1, 10
        a(1) = _lcompilers_real_i321(i)
        __libasr_created__result_metadata_ = size(a)
        reallocate(c(size(a)))
        call f3(a, c)
        s = s + c(1) + real(size(c), kind=4)
        call shrink(__lcompilers_frame)
    end do
    if (size(c) /= 1) then
        error stop
    end if
    contains
    subroutine shrink(__lcompilers_frame)
        type(__lcompilers_frame_reallocated), intent(in) :: __lcompilers_frame
        integer(4) :: __libasr_index_0_
        real(4), dimension(:), allocatable, host_associated :: c => __lcompilers_frame%c
        deallocate(c)
        allocate(c(1))
        do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_) = real(0, kind=4)
        end do
    end subroutine shrink

end subroutine reallocated

subroutine untouched(n, s)
    type :: __lcompilers_frame_untouched
        integer(4), reference :: k
    end type __lcompilers_frame_untouched
    type(__lcompilers_frame_untouched) :: __lcompilers_frame
    integer(4) :: __libasr_created__result_metadata_
    integer(4), intent(in) :: n
    integer(4) :: __libasr_extent_a_1 = Max(0, n)
    integer(4) :: __libasr_index_0_
    real(4), dimension(__libasr_extent_a_1) :: a
    real(4), dimension(:), allocatable :: c
    integer(4) :: i
    integer(4) :: k
    real(4), intent(out) :: s
    bind_reference(__lcompilers_frame%k, k)
    do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_) = real(1, kind=4)
    end do
    s = real(0, kind=4)
    k = 0
    allocate(c(size(a)))
    do i = 1, 10
        a(1) = _lcompilers_real_i322(i)
        __libasr_created__result_metadata_ = size(a)
        call f3(a, c)
        call count(__lcompilers_frame)
        s = s + c(1)
    end do
    if (k /= 10 .or. size(c) /= n) then
        error stop
    end if
    contains
    subroutine count(__lcompilers_frame)
        type(__lcompilers_frame_untouched), intent(in) :: __lcompilers_frame
        integer(4), host_associated :: k => __lcompilers_frame%k
        k = k + 1
    end subroutine count

end subroutine untouched

end module realloc_lhs_hoist_15_m

program realloc_lhs_hoist_15
use realloc_lhs_hoist_15_m, only: box_t
use realloc_lhs_hoist_15_m, only: component
use realloc_lhs_hoist_15_m, only: f3
use realloc_lhs_hoist_15_m, only: reallocated
use realloc_lhs_hoist_15_m, only: untouched
implicit none
real(4) :: s
call component(3, s)
if (Abs(s - real(120, kind=4)) > 9.99999975e-05) then
    error stop
end if
call untouched(4, s)
if (Abs(s - real(120, kind=4)) > 9.99999975e-05) then
    error stop
end if
call reallocated(2, s)
if (Abs(s - real(140, kind=4)) > 9.99999975e-05) then
    error stop
end if
print *, "ok"

contains

subroutine __libasr_intent_out_reset_box_t(self)
    use realloc_lhs_hoist_15_m, only: 1_box_t => box_t
    type(1_box_t), intent(inout) :: self
    if (Allocated(self%v)) then
        deallocate(self%v)
    end if
end subroutine __libasr_intent_out_reset_box_t

real(4) function _lcompilers_real_i32(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i32 = real(x, kind=4)
end function _lcompilers_real_i32

real(4) function _lcompilers_real_i321(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i321 = real(x, kind=4)
end function _lcompilers_real_i321

real(4) function _lcompilers_real_i322(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i322 = real(x, kind=4)
end function _lcompilers_real_i322

end program realloc_lhs_hoist_15
