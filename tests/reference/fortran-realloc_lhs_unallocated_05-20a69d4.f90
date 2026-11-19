module realloc_lhs_unallocated_05_m
implicit none

contains

subroutine first(n, s)
    real(4) :: __libasr_created__intrinsic_array_function_Sum
    real(4) :: __libasr_created__intrinsic_array_function_Sum1
    real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    real(4), dimension(:), allocatable :: t
    real(4), dimension(:), allocatable :: x
    deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
    call numbers(n, __libasr_created__subroutine_from_function_)
    if (.false.) then
        if (size(x, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(x, 1) /= 1) then
            ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_, 
            __libasr_index_0_2 = lbound(__libasr_created__subroutine_from_function_, 1)
            do __libasr_index_0_1 = lbound(x, 1), ubound(x, 1)
                x(__libasr_index_0_1) = __libasr_created__subroutine_from_function_(__libasr_index_0_2)
                __libasr_index_0_2 = __libasr_index_0_2 + 1
            end do
        else
            deallocate(x) ! Implicit deallocate
            x = __libasr_created__subroutine_from_function_
        end if
    else
        deallocate(x) ! Implicit deallocate
        x = __libasr_created__subroutine_from_function_
    end if
    if (size(x) /= n .or. lbound(x, 1) /= 1) then
        error stop
    end if
    if (.false.) then
        error stop
    end if
    if (.true.) then
        allocate(t(n + 1))
    else
        error stop
    end if
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = real(1, kind=4)
    end do
    __libasr_created__intrinsic_array_function_Sum = _lcompilers_Sum_4_1_0_idx4(x)
    __libasr_created__intrinsic_array_function_Sum1 = _lcompilers_Sum_4_1_0_idx4(t)
    s = __libasr_created__intrinsic_array_function_Sum + __libasr_created__intrinsic_array_function_Sum1
end subroutine first

subroutine fresh(o, n)
    real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(out) :: o
    if (Allocated(o)) then
        deallocate(o)
    end if
    if (.false.) then
        error stop
    end if
    deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
    call numbers(n, __libasr_created__subroutine_from_function_)
    if (.false.) then
        if (size(o, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(o, 1) /= 1) then
            ! CheckBounds LHS = o, RHS = __libasr_created__subroutine_from_function_, 
            __libasr_index_0_1 = lbound(__libasr_created__subroutine_from_function_, 1)
            do __libasr_index_0_ = lbound(o, 1), ubound(o, 1)
                o(__libasr_index_0_) = __libasr_created__subroutine_from_function_(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        else
            deallocate(o) ! Implicit deallocate
            o = __libasr_created__subroutine_from_function_
        end if
    else
        deallocate(o) ! Implicit deallocate
        o = __libasr_created__subroutine_from_function_
    end if
end subroutine fresh

subroutine maybe(flag, n)
    real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4), intent(in) :: flag
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable :: t
    if (flag) then
        allocate(t(2))
    end if
    if (Allocated(t)) then
        if (.not. flag) then
            error stop
        end if
    else
        if (flag) then
            error stop
        end if
    end if
    deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
    call numbers(n, __libasr_created__subroutine_from_function_)
    if (Allocated(t)) then
        if (size(t, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(t, 1) /= 1) then
            ! CheckBounds LHS = t, RHS = __libasr_created__subroutine_from_function_, 
            __libasr_index_0_1 = lbound(__libasr_created__subroutine_from_function_, 1)
            do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
                t(__libasr_index_0_) = __libasr_created__subroutine_from_function_(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        else
            deallocate(t) ! Implicit deallocate
            t = __libasr_created__subroutine_from_function_
        end if
    else
        deallocate(t) ! Implicit deallocate
        t = __libasr_created__subroutine_from_function_
    end if
    if (size(t) /= n) then
        error stop
    end if
end subroutine maybe

pure subroutine numbers(n, r)
    integer(4) :: __1__libasr_index_
    real(4), dimension(:), allocatable :: __libasr__created__var__0__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    deallocate(__libasr__created__var__0__array_constructor_)
    allocate(__libasr__created__var__0__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__0__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__0__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(lfortran_implied_do_i)
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(r(size(__libasr__created__var__0__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__0__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__0__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__0__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine numbers

end module realloc_lhs_unallocated_05_m

program realloc_lhs_unallocated_05
use realloc_lhs_unallocated_05_m, only: first
use realloc_lhs_unallocated_05_m, only: fresh
use realloc_lhs_unallocated_05_m, only: maybe
use realloc_lhs_unallocated_05_m, only: numbers
implicit none
real(4), dimension(:), allocatable :: o
real(4) :: s
call first(3, s)
if (Abs(s - real(10, kind=4)) > 9.99999997e-07) then
    error stop
end if
call maybe(.true., 3)
call maybe(.false., 2)
allocate(o(5))
call fresh(o, 2)
if (size(o) /= 2 .or. o(2) /= real(2, kind=4)) then
    error stop
end if
print *, "ok"

contains

real(4) function _lcompilers_Sum_4_1_0_idx4(array) result(result)
    integer(4) :: __1_i
    real(4), dimension(:), intent(in) :: array
    result = 0.00000000e+00
    do __1_i = lbound(array, 1), ubound(array, 1)
        result = result + array(__1_i)
    end do
end function _lcompilers_Sum_4_1_0_idx4

real(4) function _lcompilers_real_i32(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i32 = real(x, kind=4)
end function _lcompilers_real_i32

end program realloc_lhs_unallocated_05
