program iso_c_binding_01
      use iso_c_binding
      character(len=2, kind=c_char), parameter :: a = 'a'
      if (a /= 'a') then error stop
      if (len(a) /= 2) then error stop
      if (kind(a) /= c_char) then error stop
end program iso_c_binding_01
