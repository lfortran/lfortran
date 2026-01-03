program string_47
implicit none

    type :: string
        character(:), allocatable :: s
    end type

    type(string), allocatable :: tokens(:)
    tokens = bpe("abc")
    print *, tokens(1)%s, tokens(2)%s, tokens(3)%s
    if( tokens(1)%s /= "a" ) error stop
    if( tokens(2)%s /= "b" ) error stop
    if( tokens(3)%s /= "c" ) error stop

contains

    function merge_utf8_pairs(intokens) result(tokens)
        type(string), intent(in) :: intokens(:)
        type(string), allocatable :: tokens(:)

        allocate(tokens(size(intokens)))
        tokens = intokens
    end function

    function bpe(token) result(tokens)
        character(*), intent(in) :: token
        type(string), allocatable :: tokens(:)
        integer :: i
        allocate(tokens(len(token)))

        do i = 1, len(token)
            tokens(i)%s = token(i:i)
        end do

        tokens = merge_utf8_pairs(tokens)
    end function
end program
