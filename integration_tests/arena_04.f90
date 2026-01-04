! Test: Diamond call pattern with arena allocator
! Verifies: Parent calls child_a, child_b sequentially, both call same grandchild
! This tests arena state management with complex call graphs
program arena_04
    implicit none
    call test_diamond_pattern()
    call test_double_diamond()
    print *, "PASS: Arena diamond pattern tests passed"
contains

    ! Diamond: parent -> child_a -> grandchild
    !                 -> child_b -> grandchild
    subroutine test_diamond_pattern()
        integer :: parent_arr(100)
        integer :: i, result_a, result_b
        ! Initialize parent array
        do i = 1, 100
            parent_arr(i) = i * 7
        end do
        ! Call first child
        result_a = child_a(50)
        ! Verify parent array after child_a
        do i = 1, 100
            if (parent_arr(i) /= i * 7) then
                error stop "FAIL: Parent corrupted after child_a"
            end if
        end do
        ! Call second child (same grandchild)
        result_b = child_b(60)
        ! Verify parent array after child_b
        do i = 1, 100
            if (parent_arr(i) /= i * 7) then
                error stop "FAIL: Parent corrupted after child_b"
            end if
        end do
        ! Verify results
        if (result_a /= 2500) error stop "FAIL: child_a result wrong"
        if (result_b /= 3600) error stop "FAIL: child_b result wrong"
    end subroutine

    function child_a(n) result(res)
        integer, intent(in) :: n
        integer :: res
        integer :: child_a_arr(80)
        integer :: i
        do i = 1, 80
            child_a_arr(i) = i + n
        end do
        res = grandchild(n)
        ! Verify child_a array after grandchild
        do i = 1, 80
            if (child_a_arr(i) /= i + n) then
                error stop "FAIL: child_a_arr corrupted after grandchild"
            end if
        end do
    end function

    function child_b(n) result(res)
        integer, intent(in) :: n
        integer :: res
        integer :: child_b_arr(90)
        integer :: i
        do i = 1, 90
            child_b_arr(i) = i * n
        end do
        res = grandchild(n)
        ! Verify child_b array after grandchild
        do i = 1, 90
            if (child_b_arr(i) /= i * n) then
                error stop "FAIL: child_b_arr corrupted after grandchild"
            end if
        end do
    end function

    function grandchild(n) result(res)
        integer, intent(in) :: n
        integer :: res
        integer :: gc_arr(50)
        integer :: i
        do i = 1, 50
            gc_arr(i) = n * i
        end do
        res = n * n
        ! Verify grandchild array
        do i = 1, 50
            if (gc_arr(i) /= n * i) then
                error stop "FAIL: grandchild array corrupted"
            end if
        end do
    end function

    ! Double diamond: parent -> [child1 -> gc] -> [child2 -> gc] -> [child1 -> gc]
    subroutine test_double_diamond()
        integer :: top_arr(120)
        integer :: i, r1, r2, r3
        do i = 1, 120
            top_arr(i) = i * 11
        end do
        r1 = child_a(10)
        ! Verify top array
        do i = 1, 120
            if (top_arr(i) /= i * 11) then
                error stop "FAIL: top_arr corrupted after first child_a"
            end if
        end do
        r2 = child_b(20)
        do i = 1, 120
            if (top_arr(i) /= i * 11) then
                error stop "FAIL: top_arr corrupted after child_b"
            end if
        end do
        r3 = child_a(30)
        do i = 1, 120
            if (top_arr(i) /= i * 11) then
                error stop "FAIL: top_arr corrupted after second child_a"
            end if
        end do
        if (r1 /= 100) error stop "FAIL: r1 wrong"
        if (r2 /= 400) error stop "FAIL: r2 wrong"
        if (r3 /= 900) error stop "FAIL: r3 wrong"
    end subroutine

end program
