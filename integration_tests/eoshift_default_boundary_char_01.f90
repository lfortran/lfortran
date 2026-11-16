program eoshift_default_boundary_char_01
    ! eoshift of a character array without BOUNDARY fills every character of
    ! the shifted-in elements with blanks, for any length (#14677)
    implicit none
    character(3), parameter :: p3(4) = ['aaa', 'bbb', 'ccc', 'ddd']
    character(3), parameter :: q3(4) = eoshift(p3, 2)
    character(5), parameter :: p5(3) = ['abcde', 'fghij', 'klmno']
    character(5), parameter :: q5(3) = eoshift(p5, -1)
    character(3), parameter :: pm(2, 3) = reshape(['a11', 'a21', 'a12', &
        'a22', 'a13', 'a23'], [2, 3])
    character(1) :: a1(4), r1(4)
    character(3) :: a3(4), r3(4), m(2, 3), rm(2, 3)
    character(5) :: a5(3), r5(3)
    character(:), allocatable :: d(:), rd(:)
    integer :: s

    ! len 1, variable, positive and negative shift
    a1 = ['a', 'b', 'c', 'd']
    r1 = eoshift(a1, 1)
    if (any(r1 /= ['b', 'c', 'd', ' '])) error stop 1
    if (.not. blank(r1(4), 1)) error stop 2
    r1 = eoshift(a1, -3)
    if (r1(4) /= 'a') error stop 3
    if (.not. (blank(r1(1), 1) .and. blank(r1(2), 1) .and. blank(r1(3), 1))) error stop 4

    ! len 3, variable, positive and negative (run-time) shift
    a3 = p3
    r3 = eoshift(a3, 2)
    if (r3(1) /= 'ccc' .or. r3(2) /= 'ddd') error stop 5
    if (.not. (blank(r3(3), 3) .and. blank(r3(4), 3))) error stop 6
    s = -2
    r3 = eoshift(a3, s)
    if (r3(3) /= 'aaa' .or. r3(4) /= 'bbb') error stop 7
    if (.not. (blank(r3(1), 3) .and. blank(r3(2), 3))) error stop 8

    ! len 5, variable, positive and negative shift
    a5 = p5
    r5 = eoshift(a5, 1)
    if (r5(1) /= 'fghij' .or. r5(2) /= 'klmno') error stop 9
    if (.not. blank(r5(3), 5)) error stop 10
    r5 = eoshift(a5, -2)
    if (r5(3) /= 'abcde') error stop 11
    if (.not. (blank(r5(1), 5) .and. blank(r5(2), 5))) error stop 12

    ! PARAMETER folded at compile time
    if (q3(1) /= 'ccc' .or. q3(2) /= 'ddd') error stop 13
    if (.not. (blank(q3(3), 3) .and. blank(q3(4), 3))) error stop 14
    if (q5(2) /= 'abcde' .or. q5(3) /= 'fghij') error stop 15
    if (.not. blank(q5(1), 5)) error stop 16

    ! PARAMETER argument in an expression
    r3 = eoshift(p3, -2)
    if (r3(3) /= 'aaa' .or. r3(4) /= 'bbb') error stop 17
    if (.not. (blank(r3(1), 3) .and. blank(r3(2), 3))) error stop 18
    r5 = eoshift(p5, 2)
    if (r5(1) /= 'klmno') error stop 19
    if (.not. (blank(r5(2), 5) .and. blank(r5(3), 5))) error stop 20

    ! rank 2 with dim
    m = pm
    rm = eoshift(m, 1, dim=2)
    if (any(rm(:, 1) /= ['a12', 'a22']) .or. any(rm(:, 2) /= ['a13', 'a23'])) error stop 21
    if (.not. (blank(rm(1, 3), 3) .and. blank(rm(2, 3), 3))) error stop 22
    rm = eoshift(m, -1, dim=1)
    if (any(rm(2, :) /= ['a11', 'a12', 'a13'])) error stop 23
    if (.not. (blank(rm(1, 1), 3) .and. blank(rm(1, 2), 3) .and. blank(rm(1, 3), 3))) error stop 24
    rm = eoshift(pm, -1, dim=2)
    if (any(rm(:, 2) /= ['a11', 'a21']) .or. any(rm(:, 3) /= ['a12', 'a22'])) error stop 25
    if (.not. (blank(rm(1, 1), 3) .and. blank(rm(2, 1), 3))) error stop 26

    ! assumed-shape dummies of assumed and of fixed length
    call assumed_len(p5)
    call assumed_len(a3)
    call fixed_len(a3)

    ! deferred-length allocatable array
    allocate(character(4) :: d(3), rd(3))
    d = ['wxyz', 'stuv', 'opqr']
    rd = eoshift(d, 1)
    if (len(rd) /= 4 .or. size(rd) /= 3) error stop 27
    if (rd(1) /= 'stuv' .or. rd(2) /= 'opqr') error stop 28
    if (.not. blank(rd(3), 4)) error stop 29
    rd = eoshift(d, -2)
    if (len(rd) /= 4 .or. size(rd) /= 3) error stop 30
    if (rd(3) /= 'wxyz') error stop 31
    if (.not. (blank(rd(1), 4) .and. blank(rd(2), 4))) error stop 32

    print *, 'ok'

contains

    logical function blank(c, n)
        character(*), intent(in) :: c
        integer, intent(in) :: n
        integer :: k
        blank = len(c) == n .and. c == repeat(' ', n)
        do k = 1, len(c)
            if (iachar(c(k:k)) /= 32) blank = .false.
        end do
    end function

    subroutine assumed_len(x)
        character(*), intent(in) :: x(:)
        character(len(x)) :: r(size(x))
        integer :: n, i
        n = size(x)
        r = eoshift(x, 1)
        if (any(r(1:n - 1) /= x(2:n))) error stop 40
        if (.not. blank(r(n), len(x))) error stop 41
        r = eoshift(x, -2)
        if (any(r(3:n) /= x(1:n - 2))) error stop 42
        do i = 1, 2
            if (.not. blank(r(i), len(x))) error stop 43
        end do
    end subroutine

    subroutine fixed_len(x)
        character(3), intent(in) :: x(:)
        character(3) :: r(size(x))
        integer :: n
        n = size(x)
        r = eoshift(x, 2)
        if (any(r(1:n - 2) /= x(3:n))) error stop 50
        if (.not. (blank(r(n - 1), 3) .and. blank(r(n), 3))) error stop 51
    end subroutine

end program
