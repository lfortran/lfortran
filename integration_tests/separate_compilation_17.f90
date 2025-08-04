program separate_compilation_17

    use mod_separate_compilation_17

    implicit none

    integer(4) :: fnv_1_hasher = 1
    type(key_type) :: key

    allocate(key % value(4))
    fnv_1_hasher = fnv_1_hash( key % value )
    
    print *, fnv_1_hasher
    print *, key % value
    if (fnv_1_hasher /= 5) error stop
    if (.not. all(key % value == [1, 2, 3, 4])) error stop
end program