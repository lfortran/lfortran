program character_33
    implicit none
    integer, parameter :: marker = int(z'1f600')
    character(len=1, kind=4) :: glyph

    glyph(1:1) = achar(marker, kind=4)

    if (ichar(glyph(1:1)) /= marker) error stop
    print *, "test passed"
end program character_33
