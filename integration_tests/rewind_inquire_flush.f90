program rewind_inquire_flush
    implicit none
    integer :: ios, len, a, b
    character :: fm
    logical :: ext
    rewind(unit=9, iostat=ios, err=10)
    inquire (file='file_b', exist=ext)
    inquire (4, form=fm, iostat=ios, err=20)
    inquire (iolength=len) a, b
    10 print *, "err rewind"
    20 print *, "err inquire"
end program