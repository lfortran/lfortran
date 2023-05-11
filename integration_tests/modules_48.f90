module modules_48_fpm_strings
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

function is_fortran_name(line) result (lout)
    character(len=*), parameter :: int = '0123456789'
    character(len=*), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*), parameter :: allowed = upper // lower // int // '_'
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: name
    logical :: lout
        name = trim(line)
        if( len(name) /= 0 ) then
            lout = .true. &
             & .and. verify(name(1:1), lower//upper) == 0 &
             & .and. verify(name, allowed) == 0 &
             & .and. len(name) <= 63
        else
            lout = .false.
        endif
end function is_fortran_name

function string_cat(strings, delim) result(cat)
    type(string_t), intent(in) :: strings(:)
    character(*), intent(in), optional :: delim
    character(:), allocatable :: cat

    integer :: i
    character(:), allocatable :: delim_str

    if (size(strings) < 1) then
        cat = ''
        return
    end if

    if (present(delim)) then
        delim_str = delim
    else
        delim_str = ''
    end if

    cat = strings(1)%s
    do i=2,size(strings)

        cat = cat//delim_str//strings(i)%s

    end do

end function string_cat

end module

program modules_48
use modules_48_fpm_strings
implicit none

print *, "running modules_48"

end program
