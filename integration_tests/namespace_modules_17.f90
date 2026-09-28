! Character variables accessed through a namespace: substrings, LEN,
! deferred-length allocatable strings, concatenation.
module namespace_modules_17_text
    implicit none
    character(len=10) :: fixed = "abcdefghij"
    character(len=:), allocatable :: dynamic
contains
    function upper_first(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        r = s
        if (len(s) > 0) then
            if (s(1:1) >= "a" .and. s(1:1) <= "z") then
                r(1:1) = achar(iachar(s(1:1)) - 32)
            end if
        end if
    end function
end module

program namespace_modules_17
    use, namespace :: txt => namespace_modules_17_text
    implicit none
    character(len=:), allocatable :: s

    if (len(txt%fixed) /= 10) error stop
    if (txt%fixed(3:5) /= "cde") error stop
    txt%fixed(1:2) = "XY"
    if (txt%fixed /= "XYcdefghij") error stop

    txt%dynamic = "hello"
    if (len(txt%dynamic) /= 5) error stop
    txt%dynamic = txt%dynamic // " world"
    if (txt%dynamic /= "hello world") error stop
    if (txt%dynamic(7:) /= "world") error stop

    s = txt%upper_first(txt%dynamic)
    if (s /= "Hello world") error stop
    print *, txt%fixed, " ", s
end program
