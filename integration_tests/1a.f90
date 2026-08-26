program MRE
    character(14),save  :: f1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    real,allocatable :: x(:)
    real :: xp, xm
    integer  :: dmx
    x=[1234.5678,8765.4321,0.0,30.10e35,-123.45]
   xp = maxval(x )
   xm = minval(x )
   dmx=5
   write(f1(7:11), '(SS,I2,".",I2.2)') dmx + 8, dmx - 1
   write(*,'(*(a))') 'format=',f1
   write(*,f1) x
end program MRE