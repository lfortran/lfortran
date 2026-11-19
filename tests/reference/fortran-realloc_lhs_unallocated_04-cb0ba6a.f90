module realloc_lhs_unallocated_04_m
implicit none

contains

pure subroutine numbers(n, r)
    integer(4) :: __1__libasr_index_
    real(4), dimension(:), allocatable :: __libasr__created__var__2__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    deallocate(__libasr__created__var__2__array_constructor_)
    allocate(__libasr__created__var__2__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__2__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__2__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(lfortran_implied_do_i)
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(r(size(__libasr__created__var__2__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__2__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__2__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__2__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine numbers

end module realloc_lhs_unallocated_04_m

program realloc_lhs_unallocated_04
use realloc_lhs_unallocated_04_m, only: numbers
implicit none
real(4), dimension(4), parameter :: __libasr_array_constant_ = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00,&
         4.00000000e+00]
real(4), dimension(4), parameter :: __libasr_array_constant_1 = [4.00000000e+00, 3.00000000e+00, 2.00000000e+00,&
         1.00000000e+00]
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_index_0_1
integer(4) :: __libasr_index_0_2
integer(4) :: __libasr_index_0_3
real(4), dimension(:), allocatable :: a
real(4), dimension(:), allocatable :: b
integer(4) :: k
real(4) :: s
allocate(a(size(__libasr_array_constant_, 1)))
! CheckBounds LHS = a, RHS = __libasr_array_constant_, 
__libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
    a(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
    __libasr_index_0_1 = __libasr_index_0_1 + 1
end do
allocate(b(size(__libasr_array_constant_1, 1)))
! CheckBounds LHS = b, RHS = __libasr_array_constant_1, 
__libasr_index_0_3 = lbound(__libasr_array_constant_1, 1)
do __libasr_index_0_2 = lbound(b, 1), ubound(b, 1)
    b(__libasr_index_0_2) = __libasr_array_constant_1(__libasr_index_0_3)
    __libasr_index_0_3 = __libasr_index_0_3 + 1
end do
s = real(0, kind=4)
do k = 1, 4
    associate_block : associate (__libasr_created_real_binop_left_ => a(1:k), __libasr_created_real_binop_right_ => b(1:k))
        logical(4) :: __libasr_created__intrinsic_array_function_Any
        real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        integer(4) :: __libasr_index_0_2
        integer(4) :: __libasr_index_0_3
        integer(4) :: __libasr_index_0_4
        integer(4) :: __libasr_index_0_5
        logical(4) :: __libasr_reduction_acc
        real(4), dimension(:), allocatable :: x
        real(4), dimension(:), allocatable :: y
        allocate(x(size(__libasr_created_real_binop_left_, 1)))
        ! CheckBounds LHS = x, RHS = __libasr_created_real_binop_left_, __libasr_created_real_binop_right_, 
        __libasr_index_0_1 = lbound(__libasr_created_real_binop_left_, 1)
        __libasr_index_0_2 = lbound(__libasr_created_real_binop_right_, 1)
        do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
            x(__libasr_index_0_) = __libasr_created_real_binop_left_(__libasr_index_0_1) + __libasr_created_real_binop_r&
        ight_(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
        deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
        call numbers(k, __libasr_created__subroutine_from_function_)
        if (.false.) then
            if (size(y, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(y, 1) /= 1) then
                ! CheckBounds LHS = y, RHS = __libasr_created__subroutine_from_function_, 
                __libasr_index_0_5 = lbound(__libasr_created__subroutine_from_function_, 1)
                do __libasr_index_0_4 = lbound(y, 1), ubound(y, 1)
                    y(__libasr_index_0_4) = __libasr_created__subroutine_from_function_(__libasr_index_0_5)
                    __libasr_index_0_5 = __libasr_index_0_5 + 1
                end do
            else
                deallocate(y) ! Implicit deallocate
                y = __libasr_created__subroutine_from_function_
            end if
        else
            deallocate(y) ! Implicit deallocate
            y = __libasr_created__subroutine_from_function_
        end if
        if (size(x) /= k .or. lbound(x, 1) /= 1) then
            error stop
        end if
        __libasr_reduction_acc = .false.
        do __libasr_index_0_3 = lbound(x, 1), ubound(x, 1)
            __libasr_reduction_acc = __libasr_reduction_acc .or. x(__libasr_index_0_3) /= real(5, kind=4)
        end do
        __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
        if (__libasr_created__intrinsic_array_function_Any) then
            error stop
        end if
        if (size(y) /= k .or. y(k) /= real(k, kind=4)) then
            error stop
        end if
        associate_block : associate ()
            real(4) :: __libasr_created__intrinsic_array_function_Sum
            integer(4) :: __libasr_index_0_
            integer(4) :: __libasr_index_0_1
            integer(4) :: __libasr_index_0_2
            real(4), dimension(:), allocatable :: z
            allocate(z(size(x, 1)))
            ! CheckBounds LHS = z, RHS = x, y, 
            __libasr_index_0_1 = lbound(x, 1)
            __libasr_index_0_2 = lbound(y, 1)
            do __libasr_index_0_ = lbound(z, 1), ubound(z, 1)
                z(__libasr_index_0_) = real(2, kind=4)*x(__libasr_index_0_1) + y(__libasr_index_0_2)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
                __libasr_index_0_2 = __libasr_index_0_2 + 1
            end do
            if (size(z) /= k .or. z(1) /= real(11, kind=4)) then
                error stop
            end if
            __libasr_created__intrinsic_array_function_Sum = _lcompilers_Sum_4_1_0_idx4(z)
            s = s + __libasr_created__intrinsic_array_function_Sum
        end associate associate_block
    end associate associate_block
end do
if (Abs(s - real(11 + 23 + 36 + 50, kind=4)) > 9.99999975e-05) then
    error stop
end if
do k = 1, 3
    associate_block1 : associate ()
        real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        real(4), dimension(:), allocatable :: x
        deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
        call numbers(k + 1, __libasr_created__subroutine_from_function_)
        if (.false.) then
            if (size(x, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(x, 1) /= 1) then
                ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_, 
                __libasr_index_0_1 = lbound(__libasr_created__subroutine_from_function_, 1)
                do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
                    x(__libasr_index_0_) = __libasr_created__subroutine_from_function_(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            else
                deallocate(x) ! Implicit deallocate
                x = __libasr_created__subroutine_from_function_
            end if
        else
            deallocate(x) ! Implicit deallocate
            x = __libasr_created__subroutine_from_function_
        end if
        if (k == 2) then
            exit
        end if
        if (size(x) /= k + 1) then
            error stop
        end if
    end associate associate_block1
end do
associate_block2 : associate ()
    real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable :: x
    deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
    call numbers(2, __libasr_created__subroutine_from_function_)
    if (.false.) then
        if (size(x, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(x, 1) /= 1) then
            ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_, 
            __libasr_index_0_1 = lbound(__libasr_created__subroutine_from_function_, 1)
            do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
                x(__libasr_index_0_) = __libasr_created__subroutine_from_function_(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        else
            deallocate(x) ! Implicit deallocate
            x = __libasr_created__subroutine_from_function_
        end if
    else
        deallocate(x) ! Implicit deallocate
        x = __libasr_created__subroutine_from_function_
    end if
    if (size(x) /= 2 .or. x(2) /= real(2, kind=4)) then
        error stop
    end if
end associate associate_block2
associate_block3 : associate (x => a)
    x(1) = real(10, kind=4)
end associate associate_block3
if (a(1) /= real(10, kind=4)) then
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

end program realloc_lhs_unallocated_04
