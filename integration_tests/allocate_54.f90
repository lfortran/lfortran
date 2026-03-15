! Test allocating unlimited polymorphic arrays with character type-spec and mold
program allocate_54
    implicit none

    ! Test 1: allocate class(*) array with explicit character type-spec
    call test_char_typespec()

    ! Test 2: allocate class(*) array with mold= of character dynamic type
    call test_char_mold()

contains

subroutine test_char_typespec()
    class(*), allocatable :: arr(:)
    allocate(character(4) :: arr(2))
    select type (arr)
    type is (character(*))
        arr(1) = "foo"
        arr(2) = "bar"
        if (arr(1) /= "foo ") error stop "test_char_typespec: arr(1) mismatch"
        if (arr(2) /= "bar ") error stop "test_char_typespec: arr(2) mismatch"
    class default
        error stop "test_char_typespec: unexpected type"
    end select
    deallocate(arr)
end subroutine

subroutine test_char_mold()
    class(*), allocatable :: arr(:), mold_var
    allocate(character(4) :: mold_var)
    allocate(arr(2), mold=mold_var)
    select type (arr)
    type is (character(*))
        arr(1) = "abcd"
        arr(2) = "efgh"
        if (arr(1) /= "abcd") error stop "test_char_mold: arr(1) mismatch"
        if (arr(2) /= "efgh") error stop "test_char_mold: arr(2) mismatch"
    class default
        error stop "test_char_mold: unexpected type"
    end select
    deallocate(arr)
    deallocate(mold_var)
end subroutine

end program
