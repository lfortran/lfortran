program character_33
    implicit none

    integer, parameter :: marker = int(z'2603')
    character(len=1, kind=4) :: glyph

    glyph(1:1) = achar(marker, kind=4)
    if (ichar(glyph(1:1)) /= marker) error stop 1
    if (iachar(glyph(1:1)) /= marker) error stop 2
    print *, "slice value:", ichar(glyph(1:1))
end program
