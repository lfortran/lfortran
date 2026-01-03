program optional_01
implicit none

character(len=1) :: response = "Y"
if (lower(response) /= 'y') error stop
print *, lower(response)

contains

elemental pure function lower(str, begin, end) result (string)
    character(*), intent(in) :: str
    character(len(str)) :: string
    integer, intent(in), optional :: begin, end
    integer :: i
    integer :: ibegin, iend

    string = str
    ibegin = 1
    if (present(begin)) then
        ibegin = max(ibegin, begin)
    end if

    iend = len_trim(str)
    if (present(end)) then
        iend = min(iend,end)
    end if

    do i = ibegin, iend
        select case (str(i:i))
            case ('A':'Z')
                string(i:i) = char(iachar(str(i:i)) + 32)
            case default
        end select
    end do

end function lower

end program
