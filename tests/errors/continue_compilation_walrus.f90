program continue_compilation_walrus
    implicit none
    integer :: x
    x := 5
    y := 5
    y := 10
end program

subroutine inferred_loop_redeclaration
    implicit none
    integer :: i
    do i := 1, 2
    end do
end subroutine

subroutine inferred_loop_real
    implicit none
    do i := 1.0, 2
    end do
end subroutine

subroutine inferred_loop_array
    implicit none
    do i := [1,2], 2
    end do
end subroutine

subroutine inferred_loop_zero_step
    implicit none
    do i := 1, 2, 0
    end do
end subroutine

subroutine inferred_loop_redefinition
    implicit none
    do i := 1, 2
        i = 1
    end do
end subroutine

subroutine inferred_implied_scope
    implicit none
    arr := [(i, i := 1, 2)]
    print *, i
end subroutine

subroutine inferred_implied_real
    implicit none
    arr := [(i, i := 1.0, 2)]
end subroutine

subroutine inferred_implied_nested_index
    implicit none
    arr := [((i, i := 1, 2), i := 1, 2)]
end subroutine

subroutine inferred_implied_unknown_shape(n)
    implicit none
    integer, intent(in) :: n
    arr := [(i, i := 1, n)]
end subroutine
