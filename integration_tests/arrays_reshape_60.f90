program arrays_reshape_60
    ! reshape of a character array with order= (#17234)
    implicit none
    type :: box
        character(len=2) :: c(6)
    end type
    character(len=3) :: src(4), grid(2,2), wide(6), tall(3,2), empty(0,2)
    character(len=3), allocatable :: asrc(:), agrid(:,:)
    character(len=:), allocatable :: dsrc(:)
    character(len=3), target :: tsrc(4)
    character(len=3), pointer :: psrc(:)
    character(len=2) :: cube_src(24), cube(2,3,4), pairs(3,2)
    character(len=1) :: letters(6), lgrid(2,3)
    character(len=64) :: big_src(1024), big(32,32)
    character(len=3) :: digits
    type(box) :: b
    integer :: ord(2), ord3(3), i, j, k, n, m
    integer(8) :: ord8(2)
    integer, allocatable :: aord(:)

    src = ["one", "two", "six", "ten"]

    grid = reshape(src, [2, 2], order=[2, 1])
    if (grid(1,1) /= "one") error stop
    if (grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six") error stop
    if (grid(2,2) /= "ten") error stop

    grid = reshape(src, [2, 2], order=[1, 2])
    if (grid(1,1) /= "one" .or. grid(2,1) /= "two") error stop
    if (grid(1,2) /= "six" .or. grid(2,2) /= "ten") error stop

    ! order only known at run time
    ord = [2, 1]
    grid = reshape(src, [2, 2], order=ord)
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop
    ord = [1, 2]
    grid = reshape(src, [2, 2], order=ord)
    if (grid(1,1) /= "one" .or. grid(2,1) /= "two") error stop
    if (grid(1,2) /= "six" .or. grid(2,2) /= "ten") error stop

    ! order of another kind, and in an allocatable array
    ord8 = [2_8, 1_8]
    grid = reshape(src, [2, 2], order=ord8)
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop
    allocate(aord(2))
    aord = [2, 1]
    grid = reshape(src, [2, 2], order=aord)
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop

    ! source with more elements than the result
    wide = ["one", "two", "six", "ten", "abc", "def"]
    grid = reshape(wide, [2, 2], order=[2, 1])
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop

    ! non-square result
    tall = reshape(wide, [3, 2], order=[2, 1])
    if (tall(1,1) /= "one" .or. tall(1,2) /= "two") error stop
    if (tall(2,1) /= "six" .or. tall(2,2) /= "ten") error stop
    if (tall(3,1) /= "abc" .or. tall(3,2) /= "def") error stop

    ! source of rank 2
    grid = reshape(tall(1:2,:), [2, 2], order=[2, 1])
    if (grid(1,1) /= "one" .or. grid(1,2) /= "six") error stop
    if (grid(2,1) /= "two" .or. grid(2,2) /= "ten") error stop

    ! array section and array constructor sources
    grid = reshape(wide(2:5), [2, 2], order=[2, 1])
    if (grid(1,1) /= "two" .or. grid(1,2) /= "six") error stop
    if (grid(2,1) /= "ten" .or. grid(2,2) /= "abc") error stop
    grid = reshape(wide(5:2:-1), [2, 2], order=[2, 1])
    if (grid(1,1) /= "abc" .or. grid(1,2) /= "ten") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "two") error stop
    grid = reshape([src(3:4), "uvw", "xyz"], shape(grid), order=[2, 1])
    if (grid(1,1) /= "six" .or. grid(1,2) /= "ten") error stop
    if (grid(2,1) /= "uvw" .or. grid(2,2) /= "xyz") error stop

    ! rank 3
    do i = 1, 24
        cube_src(i) = achar(64 + i) // achar(96 + i)
    end do
    cube = reshape(cube_src, [2, 3, 4], order=[3, 1, 2])
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                if (cube(i,j,k) /= cube_src(k + 4*(i-1) + 8*(j-1))) error stop
            end do
        end do
    end do
    ord3 = [2, 3, 1]
    cube = reshape(cube_src, [2, 3, 4], order=ord3)
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                if (cube(i,j,k) /= cube_src(j + 3*(k-1) + 12*(i-1))) error stop
            end do
        end do
    end do

    ! allocatable source
    allocate(asrc(4))
    asrc = src
    grid = reshape(asrc, [2, 2], order=[2, 1])
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop
    if (any(asrc /= src)) error stop

    ! deferred length source
    allocate(character(len=3) :: dsrc(4))
    dsrc = src
    grid = reshape(dsrc, [2, 2], order=[2, 1])
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop

    ! pointer source
    tsrc = src
    psrc => tsrc
    grid = reshape(psrc, [2, 2], order=[2, 1])
    if (grid(1,1) /= "one" .or. grid(1,2) /= "two") error stop
    if (grid(2,1) /= "six" .or. grid(2,2) /= "ten") error stop

    ! result and source of size zero
    empty = reshape(src, [0, 2], order=[2, 1])
    if (size(empty) /= 0) error stop
    empty = reshape(src(1:0), [0, 2], order=[2, 1])
    if (size(empty) /= 0) error stop
    if (any(src /= ["one", "two", "six", "ten"])) error stop

    ! result of a size zero that is only known at run time
    n = 0
    if (size(reshape(src, [n, 2], order=[2, 1])) /= 0) error stop
    if (size(reshape(src, [2, n], order=[2, 1]), 1) /= 2) error stop
    if (size(reshape(src, [2, n], order=[2, 1]), 2) /= 0) error stop
    empty = reshape(src, [n, 2], order=[2, 1])
    if (any(src /= ["one", "two", "six", "ten"])) error stop

    ! shape only known at run time
    n = 3
    m = 2
    tall = reshape(wide, [n, m], order=[2, 1])
    if (tall(1,1) /= "one" .or. tall(1,2) /= "two") error stop
    if (tall(2,1) /= "six" .or. tall(2,2) /= "ten") error stop
    if (tall(3,1) /= "abc" .or. tall(3,2) /= "def") error stop
    ord = [1, 2]
    tall = reshape(wide, [n, m], order=ord)
    if (tall(1,1) /= "one" .or. tall(2,1) /= "two" .or. tall(3,1) /= "six") error stop
    if (tall(1,2) /= "ten" .or. tall(2,2) /= "abc" .or. tall(3,2) /= "def") error stop
    if (size(reshape(wide, [m, n], order=[2, 1]), 1) /= 2) error stop
    if (size(reshape(wide, [m, n], order=[2, 1]), 2) /= 3) error stop

    ! allocatable result
    allocate(agrid(3,2))
    agrid = reshape(wide, [n, m], order=[2, 1])
    if (size(agrid, 1) /= 3 .or. size(agrid, 2) /= 2) error stop
    if (agrid(1,1) /= "one" .or. agrid(1,2) /= "two") error stop
    if (agrid(2,1) /= "six" .or. agrid(2,2) /= "ten") error stop
    if (agrid(3,1) /= "abc" .or. agrid(3,2) /= "def") error stop

    ! in a long loop the result must not take more stack every time
    k = 0
    do i = 1, 1000000
        grid = reshape(src, [2, 2], order=[2, 1])
        if (grid(2,1) == "six") k = k + 1
    end do
    if (k /= 1000000) error stop
    k = 0
    do i = 1, 1000000
        grid = reshape(asrc, [2, 2], order=[2, 1])
        if (grid(1,2) == "two") k = k + 1
    end do
    if (k /= 1000000) error stop

    ! a large result takes stack only where and when it is computed: not in
    ! every invocation of a recursive procedure, and not on a path that is
    ! not taken
    digits = "400"
    read(digits, *) n
    big_src = "x"
    big_src(2) = "second"
    big_src(33) = "next row"
    big_src(1024) = "last"
    big = ""
    call deep(big_src, big, n)
    if (big(1,1) /= "x" .or. big(1,2) /= "second") error stop
    if (big(2,1) /= "next row" .or. big(32,32) /= "last") error stop
    call not_taken(src, 4, n /= 400)

    ! a BLOCK releases the stack that it takes on exit: the large result
    ! every time, but not the small one, which the next iteration uses again
    k = 0
    do i = 1, 2000
        block
            integer :: row
            row = 2
            grid = reshape(src, [2, 2], order=[2, 1])
            big = reshape(big_src, [32, 32], order=[2, 1])
            if (grid(row,1) == "six" .and. big(row,1) == "next row") k = k + 1
        end block
    end do
    if (k /= 2000) error stop

    ! single characters
    letters = ["a", "b", "c", "d", "e", "f"]
    lgrid = reshape(letters, [2, 3], order=[2, 1])
    if (lgrid(1,1) /= "a" .or. lgrid(1,2) /= "b" .or. lgrid(1,3) /= "c") error stop
    if (lgrid(2,1) /= "d" .or. lgrid(2,2) /= "e" .or. lgrid(2,3) /= "f") error stop

    ! derived type component
    b%c = ["aa", "bb", "cc", "dd", "ee", "ff"]
    pairs = reshape(b%c, [3, 2], order=[2, 1])
    if (pairs(1,1) /= "aa" .or. pairs(1,2) /= "bb") error stop
    if (pairs(2,1) /= "cc" .or. pairs(2,2) /= "dd") error stop
    if (pairs(3,1) /= "ee" .or. pairs(3,2) /= "ff") error stop

    ! reshape inside an expression and as an actual argument
    if (any(reshape(src, [2, 2], order=[2, 1]) /= &
            reshape(["one", "six", "two", "ten"], [2, 2]))) error stop
    call check_argument(reshape(src, [2, 2], order=[2, 1]))

    call explicit_shape(wide, 6, 3)
    call assumed_shape(wide)
    call integer_source([1, 2, 3, 4, 5, 6], 6, 0)

    print *, "OK"

contains

    subroutine check_argument(a)
        character(len=*), intent(in) :: a(:,:)
        if (len(a) /= 3) error stop
        if (size(a, 1) /= 2 .or. size(a, 2) /= 2) error stop
        if (a(1,1) /= "one" .or. a(1,2) /= "two") error stop
        if (a(2,1) /= "six" .or. a(2,2) /= "ten") error stop
    end subroutine

    ! size and length of the source only known at run time
    subroutine explicit_shape(a, n, m)
        integer, intent(in) :: n, m
        character(len=m), intent(in) :: a(n)
        character(len=3) :: g(2,3)
        g = reshape(a, [2, 3], order=[2, 1])
        if (g(1,1) /= "one" .or. g(1,2) /= "two" .or. g(1,3) /= "six") error stop
        if (g(2,1) /= "ten" .or. g(2,2) /= "abc" .or. g(2,3) /= "def") error stop
    end subroutine

    subroutine assumed_shape(a)
        character(len=*), intent(in) :: a(:)
        character(len=3) :: g(3,2)
        g = reshape(a, [3, 2], order=[2, 1])
        if (g(1,1) /= "one" .or. g(1,2) /= "two") error stop
        if (g(2,1) /= "six" .or. g(2,2) /= "ten") error stop
        if (g(3,1) /= "abc" .or. g(3,2) /= "def") error stop
    end subroutine

    recursive subroutine deep(a, g, depth)
        character(len=64), intent(in) :: a(1024)
        character(len=64), intent(inout) :: g(32,32)
        integer, intent(in) :: depth
        if (depth > 0) then
            call deep(a, g, depth - 1)
        else
            g = reshape(a, [32, 32], order=[2, 1])
        end if
    end subroutine

    subroutine not_taken(a, n, taken)
        integer, intent(in) :: n
        logical, intent(in) :: taken
        character(len=3), intent(in) :: a(n)
        if (taken) call check_argument(reshape(a, [2000, 2000], order=[2, 1]))
    end subroutine

    ! only the elements of the result are copied, whatever their type
    subroutine integer_source(a, n, k)
        integer, intent(in) :: n, k, a(n)
        integer :: none(0,2), g(2,2)
        none = reshape(a, [0, 2], order=[2, 1])
        if (size(reshape(a, [k, 2], order=[2, 1])) /= 0) error stop
        g = -1
        g = reshape(a, [2, 2], order=[2, 1])
        if (g(1,1) /= 1 .or. g(1,2) /= 2) error stop
        if (g(2,1) /= 3 .or. g(2,2) /= 4) error stop
    end subroutine

end program
