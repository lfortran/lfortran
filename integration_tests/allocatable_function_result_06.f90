module allocatable_function_result_06_m
    implicit none
contains
    function vector(n) result(r)
        integer, intent(in) :: n
        integer, allocatable :: r(:)
        integer :: i
        if (allocated(r)) error stop 12
        allocate(r(0))
        if (.not. allocated(r)) error stop 13
        if (size(r) /= 0) error stop 14
        deallocate(r)
        if (allocated(r)) error stop 15
        allocate(r(-2:n-3))
        if (.not. allocated(r)) error stop 1
        if (size(r) /= n) error stop 2
        if (n == 0) return
        if (lbound(r,1) /= -2) error stop 3
        do i = -2, n-3
            r(i) = i+3
        end do
    end function

    function forwarded(n) result(r)
        integer, intent(in) :: n
        integer, allocatable :: r(:)
        r = vector(n)
        if (.not. allocated(r)) error stop 16
        if (lbound(r,1) /= 1) error stop 17
        if (n == 0) return
    end function

    subroutine pair(a, b, na, nb)
        integer, intent(in) :: a(:), b(:), na, nb
        integer :: i
        if (size(a) /= na .or. size(b) /= nb) error stop 4
        do i = 1, na
            if (a(i) /= i) error stop 5
        end do
        do i = 1, nb
            if (b(i) /= i) error stop 6
        end do
    end subroutine

    function matrix(a) result(r)
        integer(8), intent(in) :: a(:)
        integer(8), allocatable :: r(:,:)
        integer :: i, j
        allocate(r(-1:0,4:6))
        if (lbound(r,1) /= -1 .or. lbound(r,2) /= 4) error stop 7
        do j = 4, 6
            do i = -1, 0
                r(i,j) = a(j-3) + int(i+1,8)*100_8
            end do
        end do
    end function

    subroutine check_matrix(a)
        integer(8), intent(in) :: a(:,:)
        integer :: i, j
        if (size(a,1) /= 2 .or. size(a,2) /= 3) error stop 8
        do j = 1, 3
            do i = 1, 2
                if (a(i,j) /= int(7-2*j,8)+int(i-1,8)*100_8) error stop 9
            end do
        end do
    end subroutine
end module

program allocatable_function_result_06
    use allocatable_function_result_06_m
    implicit none
    integer :: n, i
    integer :: base(5)
    integer(8) :: a(-3:2)
    do n = 0, 5
        call pair(vector(n), vector(5-n), n, 5-n)
        call pair(vector(size(vector(n))), vector(0), n, 0)
        call pair(forwarded(n), vector(5-n), n, 5-n)
    end do
    do i = 1, 5
        base(i) = i
    end do
    call pair(base(vector(3)), vector(2), 3, 2)
    do i = -3, 2
        a(i) = int(i+4,8)
    end do
    call check_matrix(matrix(a(1:-3:-2)))
    if (lbound(a,1) /= -3 .or. ubound(a,1) /= 2) error stop 10
    do i = -3, 2
        if (a(i) /= int(i+4,8)) error stop 11
    end do
end program
