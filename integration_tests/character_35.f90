program kind4_io

    integer, parameter :: ucs4 = selected_char_kind("ISO_10646")

    character(len=32, kind=ucs4) :: input
    character(len=64, kind=ucs4) :: output
    character(len=64, kind=ucs4) :: expected

    ! ASCII
    input = ucs4_"Hello"
    write(output, *) input
    expected = ucs4_"Hello"
    if (trim(output) /= trim(expected)) error stop

    ! Unicode (3-byte UTF-8 characters)
    input = ucs4_"你好"
    write(output, *) input
    expected = ucs4_"你好"
    if (trim(output) /= trim(expected)) error stop

    ! Mixed ASCII and Unicode
    input = ucs4_"Hello 你好"
    write(output, *) input
    expected = ucs4_"Hello 你好"
    if (trim(output) /= trim(expected)) error stop

    ! Supplementary-plane Unicode (4-byte UTF-8 character)
    input = ucs4_"😀"
    write(output, *) input
    expected = ucs4_"😀"
    if (trim(output) /= trim(expected)) error stop

end program kind4_io