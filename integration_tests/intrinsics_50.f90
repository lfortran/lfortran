program intrinsics_50

    implicit none

    character(len=1) :: strings(2, 2)
    print *, string_array_contains("1", ["1", "2", "3"])
    strings(1, 1) = "2"
    strings(1, 2) = "3"
    strings(2, 1) = "4"
    strings(2, 2) = "5"
    print *, string_array_contains_nested("1", strings)
contains
    logical function string_array_contains(search_string, array)
        character(*), intent(in) :: search_string
        character(*), intent(in) :: array(:)

        integer :: i

        string_array_contains = any([(array(i)==search_string, &
                                    i=1,size(array))])
    end function string_array_contains

    logical function string_array_contains_nested(search_string, array)
        character(*), intent(in) :: search_string
        character(*), intent(in) :: array(:, :)

        integer :: i, j

        string_array_contains_nested = any([((array(i, j)==search_string, &
                                    j=1,size(array, 2)), &
                                    i=1,size(array, 1))])
    end function string_array_contains_nested

end program intrinsics_50
