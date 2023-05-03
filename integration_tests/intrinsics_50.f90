program intrinsics_50

    implicit none
    print *, string_array_contains("1", ["1", "2", "3"])
contains
    logical function string_array_contains(search_string, array)
        character(*), intent(in) :: search_string
        character(*), intent(in) :: array(:)

        integer :: i

        string_array_contains = any([(array(i)==search_string, &
                                    i=1,size(array))])
    end function string_array_contains

end program intrinsics_50
