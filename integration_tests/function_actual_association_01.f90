program function_actual_association_01
    implicit none
    type box
        integer :: n
        integer, allocatable :: data(:)
        integer, pointer :: p
        character(4) :: label
    end type
    type(box) :: objects(1)
    integer, target :: lengths(3) = [3, 7, 9]
    integer, pointer :: saved
    integer :: calls = 0, values(2), shaped(3), i
    character(:), allocatable :: text

    text = grow_text(lengths(index_of()))
    if (calls /= 1 .or. lengths(1) /= 4) error stop 1
    if (len(text) /= 3 .or. text /= 'xxx') error stop 2

    lengths(1) = 3
    calls = 0
    values = grow_fixed(lengths(index_of()))
    if (calls /= 1 .or. lengths(1) /= 4) error stop 3
    if (any(values /= 3)) error stop 4

    lengths(1) = 3
    calls = 0
    shaped = grow_shape(lengths(index_of()))
    if (calls /= 1 .or. lengths(1) /= 4) error stop 5
    if (any(shaped /= [1, 2, 3])) error stop 6

    objects(1)%n = 3
    calls = 0
    text = grow_text(objects(index_of())%n)
    if (calls /= 1 .or. objects(1)%n /= 4) error stop 7
    if (len(text) /= 3 .or. text /= 'xxx') error stop 8

    calls = 0
    values = remember(lengths(index_of()))
    if (calls /= 1 .or. any(values /= 4)) error stop 9
    saved = 6
    if (lengths(1) /= 6) error stop 10

    lengths = [3, 7, 9]
    calls = 0
    values = update_section(lengths(index_of():3:2))
    if (calls /= 1 .or. any(values /= [3, 9])) error stop 11
    if (any(lengths /= [4, 7, 10])) error stop 12

    allocate(objects(1)%data(1))
    objects(1)%data = -1
    calls = 0
    values = replace_allocatable_inout(objects(index_of())%data)
    if (calls /= 1 .or. any(values /= [8, 9])) error stop 13
    if (size(objects(1)%data) /= 2) error stop 14
    if (any(objects(1)%data /= [8, 9])) error stop 15

    ! GNU repeats effectful selectors when deallocating an allocatable OUT actual.
    calls = 0
    i = index_of()
    values = replace_allocatable(objects(i)%data)
    if (calls /= 1 .or. any(values /= [8, 9])) error stop 26
    if (size(objects(1)%data) /= 2) error stop 27
    if (any(objects(1)%data /= [8, 9])) error stop 28

    objects(1)%p => lengths(1)
    calls = 0
    values = redirect(objects(index_of())%p)
    if (calls /= 1 .or. any(values /= 4)) error stop 16
    if (.not. associated(objects(1)%p, lengths(3))) error stop 17
    objects(1)%p = 12
    if (lengths(3) /= 12) error stop 18

    objects(1)%label = 'abcd'
    calls = 0
    values = edit_text(objects(index_of())%label(1:3))
    if (calls /= 1 .or. any(values /= 3)) error stop 19
    if (objects(1)%label /= 'xyzd') error stop 20

    lengths(1) = 3
    calls = 0
    values = extent_and_sum(grow_shape(lengths(index_of())))
    if (any(values /= [3, 6])) error stop 21
    if (calls /= 1 .or. lengths(1) /= 4) error stop 22

    calls = 0
    do i = 1, 3
        lengths(1) = i
        text = grow_text(lengths(index_of()))
        if (calls /= i .or. lengths(1) /= i + 1) error stop 23
        if (len(text) /= i) error stop 24
        if (text /= repeat('x', i)) error stop 25
    end do

    ! A zero-length result need not execute the function body.
    lengths(1) = 0
    text = grow_text(lengths(index_of()))
    if (len(text) /= 0) error stop 32

contains
    integer function index_of()
        calls = calls + 1
        index_of = 1
    end function

    function grow_text(n) result(r)
        integer, intent(inout) :: n
        character(n) :: r
        r = repeat('x', n)
        n = n + 1
    end function

    function grow_fixed(n) result(r)
        integer, intent(inout) :: n
        integer :: r(2)
        r = n
        n = n + 1
    end function

    function grow_shape(n) result(r)
        integer, intent(inout) :: n
        integer :: r(n), i
        do i = 1, n
            r(i) = i
        end do
        n = n + 1
    end function

    function remember(n) result(r)
        integer, target, intent(in) :: n
        integer :: r(2)
        saved => n
        r = n
    end function

    function update_section(n) result(r)
        integer, intent(inout) :: n(:)
        integer :: r(2)
        r = n
        n = n + 1
    end function

    function replace_allocatable(n) result(r)
        integer, allocatable, intent(out) :: n(:)
        integer :: r(2)
        allocate(n(2))
        n = [8, 9]
        r = n
    end function

    function replace_allocatable_inout(n) result(r)
        integer, allocatable, intent(inout) :: n(:)
        integer :: r(2)
        if (.not. allocated(n)) error stop 29
        if (size(n) /= 1) error stop 30
        if (any(n /= -1)) error stop 31
        deallocate(n)
        r = replace_allocatable(n)
    end function

    function redirect(n) result(r)
        integer, pointer, intent(inout) :: n
        integer :: r(2)
        r = n
        n => lengths(3)
    end function

    function edit_text(n) result(r)
        character(*), intent(inout) :: n
        integer :: r(2)
        r = len(n)
        n = 'xyz'
    end function

    function extent_and_sum(n) result(r)
        integer, intent(in) :: n(:)
        integer :: r(2)
        r = [size(n), sum(n)]
    end function
end program
