! CHARACTER arrays passed through implicit interfaces use a string descriptor
! containing the contiguous data and element length, not a full array descriptor.
! Cover fixed and runtime explicit shapes as well as an assumed-size dummy.
program implicit_interface_53
    implicit none
    character(4) :: var_name(2)
    character(10) :: words(3)
    integer :: check_arr, check_words, check_explicit, retval

    var_name(1) = 'var1'
    var_name(2) = 'var2'
    retval = check_arr(var_name)
    if (retval /= 0) error stop

    words(1) = 'First     '
    words(2) = 'Second    '
    words(3) = 'Third     '
    retval = check_words(words)
    if (retval /= 6) error stop

    retval = check_explicit(words, 3)
    if (retval /= 6) error stop

    print *, "OK"
end program implicit_interface_53

integer function check_arr(var_name)
    implicit none
    character(4) :: var_name(2)
    check_arr = 0
    if (var_name(1) /= 'var1') check_arr = check_arr + 1
    if (var_name(2) /= 'var2') check_arr = check_arr + 2
end function check_arr

integer function check_words(words)
    implicit none
    character(*) :: words(*)
    check_words = 0
    if (words(1)(1:5) == 'First') check_words = check_words + 1
    if (words(2)(1:6) == 'Second') check_words = check_words + 2
    if (words(3)(1:5) == 'Third') check_words = check_words + 3
end function check_words

integer function check_explicit(words, n)
    implicit none
    integer, intent(in) :: n
    character(*), intent(in) :: words(n)
    check_explicit = 0
    if (len(words) /= 10) error stop
    if (size(words) /= 3) error stop
    if (words(1)(1:5) == 'First') check_explicit = check_explicit + 1
    if (words(2)(1:6) == 'Second') check_explicit = check_explicit + 2
    if (words(3)(1:5) == 'Third') check_explicit = check_explicit + 3
end function check_explicit
