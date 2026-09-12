program implied_do_len_trim_12666
integer                      :: i
integer,parameter            :: opl(3)= [(len_trim('123456'),i=1,3)]
   write(*,*)opl
end program implied_do_len_trim_12666
