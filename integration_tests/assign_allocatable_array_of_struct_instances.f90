program assign_allocatable_array_of_struct_instances
    implicit none

    integer :: i

    type :: string
        character(:), allocatable :: s
    end type

    type(string), allocatable :: tokens(:)

    allocate(tokens(5))
    do i = 1, 5
        tokens(i)%s = "a"
    end do

    tokens = merge_pair(tokens, 1)

contains

    function merge_pair(intokens, idx) result(tkns)
        ! Merge the pair `idx`
        type(string), intent(in) :: intokens(:)
        integer, intent(in) :: idx
        type(string), allocatable :: tkns(:)
        type(string) :: merged_token
        merged_token%s = intokens(idx)%s // intokens(idx+1)%s
        ! The segmentation fault occurs at the below line, when used with
        ! `--experimental-simplifier` flag (i.e. simplifier pass enabled) and
        ! no `--realloc-lhs` flag is enabled for it
        tkns = [intokens(:idx-1), merged_token, intokens(idx+2:)]
    end function

end program
