program char4_string_literal_01
    implicit none
    integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
    ! A kind 4 literal written directly in the (UTF-8) source is decoded into
    ! code points, so its length is a count of characters. This matches LLVM
    ! Flang; GFortran instead treats each source byte as one character, so this
    ! test is not run against GFortran.
    character(kind=ucs4, len=*), parameter :: greeting = ucs4_"你好"
    character(kind=ucs4, len=:), allocatable :: copy

    if (len(greeting) /= 2) error stop 1
    if (ichar(greeting(1:1), kind=4) /= int(z'4F60')) error stop 2
    if (ichar(greeting(2:2), kind=4) /= int(z'597D')) error stop 3
    if (greeting /= char(int(z'4F60'), ucs4) // char(int(z'597D'), ucs4)) error stop 4

    copy = greeting
    if (len(copy) /= 2) error stop 5
    if (len_trim(greeting // ucs4_"  ") /= 2) error stop 6

    print *, "ok"
end program
