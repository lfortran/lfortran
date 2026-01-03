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
    end if
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

character(len=40) :: char_str
character(len=:), allocatable :: char_str_alloc, cat_str_alloc
integer(8) :: char_str_hash1, char_str_hash2
integer(8) :: char_str_hash3, char_str_hash4
type(string_t) :: string_var(2)

allocate(character(len=40) :: char_str_alloc)

char_str_alloc = "runningmodules_48_1"
string_var(1)%s = char_str_alloc
char_str = "runningmodules_48_2"
string_var(2)%s = char_str

char_str_hash1 = fnv_1a(char_str, 2166136261_8)
char_str_hash2 = fnv_1a(char_str)
print *, char_str_hash1, char_str_hash2
if( char_str_hash1 /= char_str_hash2 ) error stop

char_str_hash3 = fnv_1a_string_t(string_var, 2166136261_8)
char_str_hash4 = fnv_1a_string_t(string_var)
print *, char_str_hash3, char_str_hash4
if( char_str_hash3 /= char_str_hash4 ) error stop

cat_str_alloc = string_cat(string_var, ":")
print *, cat_str_alloc, is_fortran_name(cat_str_alloc)
if( cat_str_alloc /= "runningmodules_48_1:runningmodules_48_2" ) error stop
if( is_fortran_name(cat_str_alloc) ) error stop

cat_str_alloc = string_cat(string_var)
print *, cat_str_alloc, is_fortran_name(cat_str_alloc)
if( cat_str_alloc /= "runningmodules_48_1runningmodules_48_2" ) error stop
if( .not. is_fortran_name(cat_str_alloc) ) error stop

end program
