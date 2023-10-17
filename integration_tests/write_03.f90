subroutine dvout()
implicit none
character(80) line
integer ifmt, lll
ifmt = 10
lll = 15
line = "1234567890123456789001234567890"
write( *, fmt = 9999 ) ifmt, line( 1: lll )
9999 format( / 1x, a, / 1x, a )
end subroutine

program write_03
implicit none
call dvout()
end program


