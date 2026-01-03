program intrinsics_50
implicit none

    logical :: is_one_present, is_five_present
    character(len=1) :: strings(2, 2)

    character(len=80), parameter :: help_text_build_common(*) = [character(len=80) ::      &
        ' --profile PROF    Selects the compilation profile for the build.               ',&
        '                   Currently available profiles are "release" for               ',&
        '                   high optimization and "debug" for full debug options.        ',&
        '                   If --flag is not specified the "debug" flags are the         ',&
        '                   default.                                                     ',&
        ' --no-prune        Disable tree-shaking/pruning of unused module dependencies   '&
        ]


    is_one_present = string_array_contains("1", ["2", "3", "1"])
    print *, is_one_present
    if( .not. is_one_present ) error stop

    is_one_present = string_array_contains("1", ["2", "3"])
    print *, is_one_present
    if( is_one_present ) error stop

    strings(1, 1) = "2"
    strings(1, 2) = "3"
    strings(2, 1) = "4"
    strings(2, 2) = "5"

    is_one_present = string_array_contains_nested("1", strings)
    print *, is_one_present
    if( is_one_present ) error stop

    is_five_present = string_array_contains_nested("5", strings)
    print *, is_five_present
    if( .not. is_five_present ) error stop

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
