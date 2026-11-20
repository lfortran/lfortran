module do_loop_23_mod
    implicit none
contains
    subroutine sub_continue_comment_end(s)
        integer, intent(out) :: s
        integer :: j
        s = 0
        do 248 j = 1, 10
            s = s + j
248     continue                  ! comment directly before end subroutine
    end subroutine sub_continue_comment_end

    subroutine sub_continue_comment_after(s, after_count)
        integer, intent(out) :: s, after_count
        integer :: j
        s = 0
        after_count = 0
        do 249 j = 1, 10
            s = s + j
249     continue                  ! comment after labeled continue
        after_count = after_count + 1
    end subroutine sub_continue_comment_after
end module do_loop_23_mod

program do_loop_23
    use do_loop_23_mod, only: sub_continue_comment_end, sub_continue_comment_after
    implicit none
    integer :: i, j, s, after_count

    ! 1. Labeled do with continue ! comment in a module subroutine (issue #18028)
    call sub_continue_comment_end(s)
    if (s /= 55) error stop
    call sub_continue_comment_after(s, after_count)
    if (s /= 55 .or. after_count /= 1) error stop

    ! 2. Labeled do with continue ! comment in main program
    s = 0
    after_count = 0
    do 10 i = 1, 3
        s = s + i
10  continue ! trailing comment
    after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop

    ! 3. Labeled do with end do ! comment and enddo ! comment
    s = 0
    after_count = 0
    do 20 i = 1, 3
        s = s + i
20  end do ! trailing comment
    after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop

    s = 0
    after_count = 0
    do 25 i = 1, 3
        s = s + i
25  enddo ! trailing comment
    after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop

    ! 4. Comma form do 30, i = 1, 3 with continue ! comment
    s = 0
    after_count = 0
    do 30, i = 1, 3
        s = s + i
30  continue ! trailing comment
    after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop

    ! 5. Tab before ! comment
    s = 0
    after_count = 0
    do 40 i = 1, 3
        s = s + i
40  continue	! tab before comment
    after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop

    ! 6. Nested labeled do loops sharing a terminal label with trailing comment
    s = 0
    after_count = 0
    do 50 i = 1, 3
        do 50 j = 1, 2
            s = s + i * j
50  continue ! shared label trailing comment
    after_count = after_count + 1
    if (s /= 18 .or. after_count /= 1) error stop

    ! 7. Non-continue terminal statement with trailing comment
    s = 0
    after_count = 0
    do 60 i = 1, 4
60      s = s + i ! assignment terminal statement with comment
    after_count = after_count + 1
    if (s /= 10 .or. after_count /= 1) error stop

    ! 8. Semicolon after labeled do terminal statement
    s = 0
    after_count = 0
    do 70 i = 1, 3
        s = s + i
70  continue; after_count = after_count + 1
    if (s /= 6 .or. after_count /= 1) error stop
end program do_loop_23
