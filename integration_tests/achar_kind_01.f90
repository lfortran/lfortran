program achar_kind_01
    implicit none
    character(len=1, kind=4) :: glyph
    glyph(1:1) = achar(9731, kind=4)
    if (ichar(glyph(1:1)) /= 9731) error stop
end program
