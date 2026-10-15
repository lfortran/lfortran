program realloc_lhs_30
    implicit none
    character(8), allocatable :: result(:)
    character(:), allocatable :: deferred(:)
    integer :: length

    call fill(result)
    if (len(result) /= 8 .or. size(result) /= 2) error stop "caller shape or length"
    if (any(result /= ["ghi     ", "abc     "])) error stop "caller contents"
    if (len_trim(result(1)) /= 3) error stop "caller padding"
    call section(result)
    if (len(result) /= 8 .or. size(result) /= 1) error stop "section caller length"
    if (result(1) /= "def" .or. len_trim(result(1)) /= 3) error stop "section caller content"
    call fixed(result)
    if (len(result) /= 8 .or. size(result) /= 2) error stop "fixed caller length"
    length = 8
    call explicit(result, length)
    if (len(result) /= 8 .or. result(1) /= "abc") error stop "explicit caller content"

    deferred = ["abc", "def"]
    if (len(deferred) /= 3 .or. size(deferred) /= 2) error stop "deferred initial length"
    deferred = ["longer"]
    if (len(deferred) /= 6 .or. size(deferred) /= 1) error stop "deferred changed length"
    if (deferred(1) /= "longer") error stop "deferred content"
contains
    subroutine fill(values)
        character(*), allocatable, intent(out) :: values(:)
        character(3) :: source(3) = ["abc", "def", "ghi"]
        integer :: indices(2) = [3, 1]
        allocate(values(1))
        values = ["abc"]
        if (len(values) /= 8) error stop "constructor dummy length"
        if (values(1)(4:8) /= "     ") error stop "constructor padding"
        values = source
        if (len(values) /= 8 .or. size(values) /= 3) error stop "whole array length or size"
        if (any(values /= ["abc     ", "def     ", "ghi     "])) error stop "whole array content"
        values = source(indices)
        if (len(values) /= 8 .or. size(values) /= 2) error stop "vector subscript length or size"
        if (any(values /= ["ghi     ", "abc     "])) error stop "vector subscript content"
    end subroutine

    subroutine section(values)
        character(*), allocatable, optional, intent(out) :: values(:)
        character(3) :: source(2) = ["abc", "def"]
        if (present(values)) then
            allocate(values(2))
            values = source(2:2)
            if (len(values) /= 8 .or. size(values) /= 1) error stop "section dummy length or size"
            if (values(1)(4:8) /= "     ") error stop "section padding"
        end if
    end subroutine

    subroutine fixed(values)
        character(8), allocatable, intent(out) :: values(:)
        allocate(values(1))
        values = ["abc", "def"]
        if (len(values) /= 8 .or. size(values) /= 2) error stop "fixed dummy length or size"
        if (values(2) /= "def" .or. len_trim(values(2)) /= 3) error stop "fixed padding"
    end subroutine

    subroutine explicit(values, length)
        integer, intent(in) :: length
        character(length), allocatable, intent(out) :: values(:)
        allocate(values(1))
        values = ["abc"]
        if (len(values) /= length) error stop "explicit runtime length"
        if (values(1)(4:length) /= "     ") error stop "explicit padding"
    end subroutine
end program
