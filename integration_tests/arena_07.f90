! Test: BLOCK locals in loops with arena allocator
! Verifies: (1) BLOCK locals are properly cleaned up each iteration
!           (2) No memory leak in loops with BLOCK constructs
!           (3) Nested BLOCKs and nested loops work correctly
program arena_07
    implicit none
    call test_simple_block_loop()
    call test_nested_block_loop()
    call test_block_with_function_call()
    call test_large_block_loop()
    print *, "PASS: Arena BLOCK loop tests passed"
contains

    ! Test 1: Simple BLOCK in loop
    subroutine test_simple_block_loop()
        integer :: i, sum
        sum = 0
        do i = 1, 1000
            block
                integer :: arr(100)
                integer :: j
                do j = 1, 100
                    arr(j) = i + j
                end do
                sum = sum + arr(50)
            end block
        end do
        ! sum = sum of (i + 50) for i = 1 to 1000 = 1000*50 + 1000*1001/2 = 50000 + 500500
        if (sum /= 550500) then
            error stop "FAIL: simple block loop sum incorrect"
        end if
    end subroutine

    ! Test 2: Nested BLOCKs in nested loops
    subroutine test_nested_block_loop()
        integer :: i, j, total
        total = 0
        do i = 1, 100
            block
                integer :: outer_arr(50)
                outer_arr(1) = i
                do j = 1, 100
                    block
                        integer :: inner_arr(30)
                        inner_arr(1) = j
                        total = total + outer_arr(1) + inner_arr(1)
                    end block
                end do
            end block
        end do
        ! total = sum over i=1..100, j=1..100 of (i + j)
        ! = 100 * sum(j=1..100) + 100 * sum(i=1..100)
        ! = 100 * 5050 + 100 * 5050 = 1010000
        if (total /= 1010000) then
            error stop "FAIL: nested block loop total incorrect"
        end if
    end subroutine

    ! Test 3: BLOCK with function call inside loop
    subroutine test_block_with_function_call()
        integer :: i, result
        result = 0
        do i = 1, 500
            block
                integer :: local_arr(200)
                integer :: func_result(10)
                local_arr(1) = i
                func_result = compute_array(i)
                result = result + func_result(1) + local_arr(1)
            end block
        end do
        ! result = sum of (i*1 + i) for i = 1..500 = 2 * sum(1..500) = 2 * 125250
        if (result /= 250500) then
            error stop "FAIL: block with function call result incorrect"
        end if
    end subroutine

    function compute_array(n) result(arr)
        integer, intent(in) :: n
        integer :: arr(10)
        integer :: i
        do i = 1, 10
            arr(i) = n * i
        end do
    end function

    ! Test 4: Large iterations to verify no memory leak
    ! 10000 iterations * 4000 bytes per BLOCK = 40MB if leaked
    subroutine test_large_block_loop()
        integer :: i, check
        check = 0
        do i = 1, 10000
            block
                integer :: big_arr(1000)
                big_arr(1) = i
                big_arr(1000) = i * 2
                check = check + big_arr(1) + big_arr(1000)
            end block
        end do
        ! check = sum of (i + 2*i) for i = 1..10000 = 3 * sum(1..10000) = 3 * 50005000
        if (check /= 150015000) then
            error stop "FAIL: large block loop check incorrect"
        end if
    end subroutine

end program
