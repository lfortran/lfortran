module string_116_mod
    implicit none
contains
    function get_unallocated() result(s)
        character(len=:), allocatable :: s
    end function get_unallocated
end module string_116_mod

program string_116
    use string_116_mod
    implicit none
    character(len=32) :: blank32
    character(len=8)  :: fixed_dst
    character(len=:), allocatable :: defer_dst
    character(len=16) :: buf
    integer :: l
    fixed_dst = get_unallocated()
    if (len_trim(fixed_dst) /= 0) &
         error stop "fixed_dst must be blank after copy from unallocated"

    ! 2. Same shape, but into a deferred-length allocatable destination.
    defer_dst = get_unallocated()
    if (len(defer_dst) /= 0) &
         error stop "defer_dst must have length 0 after copy from unallocated"

    ! Force `blank32` to be all blanks so `trim(blank32)` is empty.
    blank32 = ''

    l = len_trim(blank32)
    if (l /= 0) error stop "len_trim of all blanks must be 0"

    ! 3. trim(...) -> fixed-length: result is space-padded empty string.
    fixed_dst = trim(blank32)
    if (len_trim(fixed_dst) /= 0) error stop "trim -> fixed must be blank"

    ! 4. trim(...) -> deferred-length allocatable: result has length 0.
    defer_dst = trim(blank32)
    if (len(defer_dst) /= 0) error stop "trim -> deferred must be length 0"

    ! 5. Runtime-zero substring `blank32(1:l)` with l == 0.
    fixed_dst = blank32(1:l)
    if (len_trim(fixed_dst) /= 0) error stop "substr(1:0) -> fixed must be blank"

    defer_dst = blank32(1:l)
    if (len(defer_dst) /= 0) error stop "substr(1:0) -> deferred must be length 0"

    ! 6. Use the empty trim result inside an internal WRITE.
    write(buf, '(A,A,A)') '[', trim(blank32), ']'
    if (buf(1:2) /= '[]') error stop "internal WRITE with empty trim broken"

    ! 7. Concatenation with empty trim.
    defer_dst = trim(blank32) // 'x' // trim(blank32)
    if (defer_dst /= 'x') error stop "concat with empty trim broken"
end program string_116
