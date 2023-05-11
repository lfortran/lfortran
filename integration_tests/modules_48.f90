module fpm_strings
implicit none

type string_t
    character(len=:), allocatable :: s
end type

interface fnv_1a
    procedure :: fnv_1a_char
    procedure :: fnv_1a_string_t
end interface fnv_1a

contains

pure function fnv_1a_char(input, seed) result(hash)
    character(*), intent(in) :: input
    integer(8), intent(in), optional :: seed
    integer(8) :: hash

    integer :: i
    integer(8), parameter :: FNV_OFFSET_32 = 2166136261_8
    integer(8), parameter :: FNV_PRIME_32 = 16777619_8

    if (present(seed)) then
        hash = seed
    else
        hash = FNV_OFFSET_32
    end if

    do i = 1, len(input)
        hash = ieor(hash, iachar(input(i:i), 8)) * FNV_PRIME_32
    end do

end function fnv_1a_char

pure function fnv_1a_string_t(input, seed) result(hash)
    type(string_t), intent(in) :: input(:)
    integer(8), intent(in), optional :: seed
    integer(8) :: hash

    integer :: i

    hash = fnv_1a(input(1)%s,seed)

    do i = 2, size(input)
        hash = fnv_1a(input(i)%s,hash)
    end do

end function fnv_1a_string_t

end module
