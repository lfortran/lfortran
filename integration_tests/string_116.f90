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

end module string_116_mod

program string_116
    use string_116_mod
    implicit none

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

    print *, "All tests passed."
end program string_116
