! Test: Functions returning FixedSizeArray with arena allocator
! Verifies: (1) Function return values using arena allocation
!           (2) Caller local arrays coexisting with returned arrays
!           (3) Nested function calls with multiple array returns
!           (4) No corruption between caller/callee arrays
program arena_06
    implicit none
    call test_simple_return()
    call test_caller_has_locals()
    call test_nested_returns()
    call test_chained_returns()
    print *, "PASS: Arena function return tests passed"
contains

    ! Test 1: Simple function returning an array
    subroutine test_simple_return()
        integer :: result(10)
        integer :: i
        result = make_array(5)
        do i = 1, 10
            if (result(i) /= i * 5) then
                error stop "FAIL: simple return value incorrect"
            end if
        end do
    end subroutine

    function make_array(multiplier) result(arr)
        integer, intent(in) :: multiplier
        integer :: arr(10)
        integer :: i
        do i = 1, 10
            arr(i) = i * multiplier
        end do
    end function

    ! Test 2: Caller has local arrays alongside returned array
    subroutine test_caller_has_locals()
        integer :: local_arr(100)
        integer :: returned(50)
        integer :: i
        ! Initialize caller local array
        do i = 1, 100
            local_arr(i) = i * 7
        end do
        ! Call function that returns array
        returned = get_sequence(50, 3)
        ! Verify caller local was not corrupted
        do i = 1, 100
            if (local_arr(i) /= i * 7) then
                error stop "FAIL: caller local_arr corrupted after function return"
            end if
        end do
        ! Verify returned array
        do i = 1, 50
            if (returned(i) /= i * 3) then
                error stop "FAIL: returned array value incorrect"
            end if
        end do
    end subroutine

    function get_sequence(n, factor) result(arr)
        integer, intent(in) :: n, factor
        integer :: arr(50)
        integer :: i
        do i = 1, n
            arr(i) = i * factor
        end do
    end function

    ! Test 3: Nested function calls with multiple returns
    subroutine test_nested_returns()
        integer :: outer_local(80)
        integer :: final_result(20)
        integer :: i
        ! Initialize outer local
        do i = 1, 80
            outer_local(i) = i * 11
        end do
        ! Call outer function which calls inner function
        final_result = outer_func(4)
        ! Verify outer local not corrupted
        do i = 1, 80
            if (outer_local(i) /= i * 11) then
                error stop "FAIL: outer_local corrupted after nested returns"
            end if
        end do
        ! Verify final result (inner returns i*2, outer adds base=4)
        do i = 1, 20
            if (final_result(i) /= i * 2 + 4) then
                error stop "FAIL: nested return result incorrect"
            end if
        end do
    end subroutine

    function outer_func(base) result(arr)
        integer, intent(in) :: base
        integer :: arr(20)
        integer :: func_local(60)
        integer :: inner_arr(20)
        integer :: i
        ! Outer function has its own local array
        do i = 1, 60
            func_local(i) = i * 13
        end do
        ! Call inner function
        inner_arr = inner_func(2)
        ! Verify func_local not corrupted
        do i = 1, 60
            if (func_local(i) /= i * 13) then
                error stop "FAIL: outer_func local corrupted after inner call"
            end if
        end do
        ! Build result from inner result plus base
        do i = 1, 20
            arr(i) = inner_arr(i) + base
        end do
    end function

    function inner_func(multiplier) result(arr)
        integer, intent(in) :: multiplier
        integer :: arr(20)
        integer :: inner_local(40)
        integer :: i
        ! Inner function also has local array
        do i = 1, 40
            inner_local(i) = i * 17
        end do
        ! Build return array
        do i = 1, 20
            arr(i) = i * multiplier
        end do
        ! Verify inner local before return
        do i = 1, 40
            if (inner_local(i) /= i * 17) then
                error stop "FAIL: inner_func local corrupted"
            end if
        end do
    end function

    ! Test 4: Chain of function calls with returns at each level
    subroutine test_chained_returns()
        integer :: top_local(70)
        real :: result_a(30)
        real :: result_b(30)
        integer :: i
        ! Initialize top local
        do i = 1, 70
            top_local(i) = i * 19
        end do
        ! Call chain: level1 -> level2 -> level3, each returns array
        result_a = level1_func(1.5)
        ! Verify top local not corrupted
        do i = 1, 70
            if (top_local(i) /= i * 19) then
                error stop "FAIL: top_local corrupted after first chain"
            end if
        end do
        ! Second chain call
        result_b = level1_func(2.5)
        ! Verify top local again
        do i = 1, 70
            if (top_local(i) /= i * 19) then
                error stop "FAIL: top_local corrupted after second chain"
            end if
        end do
        ! Verify results
        ! level3 returns i*1.0, level2 adds factor, level1 adds factor
        ! result_a: i*1.0 + 1.5 + 1.5 = i + 3.0
        ! result_b: i*1.0 + 2.5 + 2.5 = i + 5.0
        do i = 1, 30
            if (abs(result_a(i) - (real(i) + 3.0)) > 0.001) then
                error stop "FAIL: chained result_a incorrect"
            end if
            if (abs(result_b(i) - (real(i) + 5.0)) > 0.001) then
                error stop "FAIL: chained result_b incorrect"
            end if
        end do
    end subroutine

    function level1_func(factor) result(arr)
        real, intent(in) :: factor
        real :: arr(30)
        real :: l1_local(50)
        real :: from_l2(30)
        integer :: i
        do i = 1, 50
            l1_local(i) = real(i) * 2.0
        end do
        from_l2 = level2_func(factor)
        do i = 1, 50
            if (abs(l1_local(i) - real(i) * 2.0) > 0.001) then
                error stop "FAIL: l1_local corrupted"
            end if
        end do
        do i = 1, 30
            arr(i) = from_l2(i) + factor
        end do
    end function

    function level2_func(factor) result(arr)
        real, intent(in) :: factor
        real :: arr(30)
        real :: l2_local(45)
        real :: from_l3(30)
        integer :: i
        do i = 1, 45
            l2_local(i) = real(i) * 3.0
        end do
        from_l3 = level3_func()
        do i = 1, 45
            if (abs(l2_local(i) - real(i) * 3.0) > 0.001) then
                error stop "FAIL: l2_local corrupted"
            end if
        end do
        do i = 1, 30
            arr(i) = from_l3(i) + factor
        end do
    end function

    function level3_func() result(arr)
        real :: arr(30)
        real :: l3_local(35)
        integer :: i
        do i = 1, 35
            l3_local(i) = real(i) * 4.0
        end do
        do i = 1, 30
            arr(i) = real(i) * 1.0
        end do
        do i = 1, 35
            if (abs(l3_local(i) - real(i) * 4.0) > 0.001) then
                error stop "FAIL: l3_local corrupted"
            end if
        end do
    end function

end program
