! Test: character variable with length from type-bound member via class argument
! This tests the codegen path where a string's length is an ExpressionLength
! derived from a StructInstanceMember (e.g., me%n) accessed through a
! polymorphic class(*) self argument.

module string_116_mod
    implicit none

    type :: buffer_type
        integer :: buf_size = 10
    contains
        procedure :: fill_buffer
        procedure :: get_greeting
        procedure :: test_modify_mid_sub
    end type buffer_type

contains

    subroutine fill_buffer(me)
        class(buffer_type), intent(in) :: me
        character(len=me%buf_size) :: buf
        buf = "hello"
        if (len(buf) /= me%buf_size) error stop
        if (buf(1:5) /= "hello") error stop
    end subroutine fill_buffer

    function get_greeting(me) result(res)
        class(buffer_type), intent(in) :: me
        character(len=me%buf_size) :: res
        res = "greetings!"
    end function get_greeting

    ! Test: modifying buf_size mid-subroutine should NOT change len(buf)
    ! The character length is frozen at subroutine entry.
    subroutine test_modify_mid_sub(me)
        class(buffer_type), intent(inout) :: me
        character(len=me%buf_size) :: buf
        integer :: original_size
        original_size = me%buf_size
        buf = "hello"
        if (len(buf) /= original_size) error stop
        me%buf_size = 3
        ! len(buf) must still reflect the original value, not 3
        if (len(buf) /= original_size) error stop
        me%buf_size = original_size  ! restore
    end subroutine test_modify_mid_sub

end module string_116_mod

! Test: character(len=n) where n is a normal variable (not a struct member)
subroutine test_normal_variable(n)
    implicit none
    integer, intent(in) :: n
    character(len=n) :: buf
    buf = "hello world"
    if (len(buf) /= n) error stop
end subroutine test_normal_variable

program string_116
    use string_116_mod
    implicit none

    interface
        subroutine test_normal_variable(n)
            integer, intent(in) :: n
        end subroutine test_normal_variable
    end interface

    type(buffer_type) :: obj
    type(buffer_type) :: obj2
    character(len=10) :: result

    ! Test 1: default buf_size (10)
    call obj%fill_buffer()

    ! Test 2: custom buf_size
    obj2%buf_size = 5
    call obj2%fill_buffer()

    ! Test 3: function returning character(len=me%buf_size)
    result = obj%get_greeting()
    if (result /= "greetings!") error stop

    ! Test 4: modifying buf_size mid-subroutine (length should be frozen)
    obj%buf_size = 10
    call obj%test_modify_mid_sub()

    ! Test 5: after modification, next call should use new value
    obj2%buf_size = 7
    call obj2%fill_buffer()

    ! Test 6: character(len=n) with a normal variable
    call test_normal_variable(10)
    call test_normal_variable(5)

    print *, "All tests passed."
end program string_116
