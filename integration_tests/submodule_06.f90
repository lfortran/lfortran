module mod_submodule_06

    implicit none

    type :: key_type
        integer(1), allocatable :: value(:)
    end type key_type

    interface fnv_1_hash
        module function int8_fnv_1( key ) result(hash_code)
            integer(1), intent(inout) :: key(:)
            integer(4)             :: hash_code
        end function int8_fnv_1
    end interface fnv_1_hash

end module mod_submodule_06


submodule(mod_submodule_06) submod_submodule_06

    implicit none

contains

    module function int8_fnv_1( key ) result(hash_code)
        integer(1), intent(inout) :: key(:)
        integer(4)             :: hash_code
        integer(1), parameter :: array(4) = [1, 2, 3, 4]
        integer(4), parameter :: result = 5
        hash_code = result
        key = array
    end function int8_fnv_1

end submodule submod_submodule_06

program submodule_06

    use mod_submodule_06

    implicit none

    integer(4) :: fnv_1_hasher = 1
    type(key_type) :: key
    integer(1) :: test_object(4)

    allocate(key % value(4))
    fnv_1_hasher = fnv_1_hash( key % value )
    
    print *, fnv_1_hasher
    print *, key % value
    if (fnv_1_hasher /= 5) error stop
    if (.not. all(key % value == [1, 2, 3, 4])) error stop

    call test_fnv_1()

contains

    subroutine test_fnv_1()
        integer(4) :: hash
        hash = fnv_1_hash( test_object )

        print *, hash
        print *, test_object
        if (hash /= 5) error stop
        if (.not. all(test_object == [1, 2, 3, 4])) error stop
    end subroutine test_fnv_1
end program