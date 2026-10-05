module traits_dependent_result_oracle_m
    implicit none

    type :: Box
        integer :: value
    end type
contains
    pure function box_text(k, letters, self) result(r)
        integer, intent(in) :: k
        character(*), intent(in) :: letters
        class(Box), intent(in) :: self
        character(len=k) :: r
        r = letters
    end function

    pure function box_copy_text(letters, self, k) result(r)
        character(*), intent(in) :: letters
        class(Box), intent(in) :: self
        integer, intent(in) :: k
        character(len=k) :: r
        r = letters
    end function

    pure function box_values(offset, self, count) result(r)
        integer, intent(in) :: offset, count
        class(Box), intent(in) :: self
        integer :: r(count)
        integer :: i
        do i = 1, count
            r(i) = self%value + offset + i
        end do
    end function

    subroutine check_text(x, n)
        type(Box), intent(in) :: x
        integer, intent(in) :: n
        character(len=4) :: r
        r = box_text(n, "abcdef", x)
        if (r /= "abcd") error stop 1
        r = box_copy_text("ghijkl", x, n-1)
        if (r /= "ghi ") error stop 2
    end subroutine

    function total(x, offset, n) result(r)
        type(Box), intent(in) :: x
        integer, intent(in) :: offset, n
        integer :: r
        r = sum(box_values(offset, x, n))
    end function
end module

program traits_dependent_result_oracle
    use traits_dependent_result_oracle_m
    implicit none
    type(Box) :: x
    integer :: n
    x = Box(11)
    n = 4
    call check_text(x, n)
    n = 3
    if (total(x, 5, n) /= 54) error stop 3
    n = 5
    if (total(x, 5, n) /= 95) error stop 4
end program
