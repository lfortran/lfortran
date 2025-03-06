program iso_c_binding_01
      use iso_c_binding
      character(len=2, kind=c_char), parameter :: a = 'a'
      if (a /= 'a') error stop
      if (len(a) /= 2) error stop
      if (kind(a) /= c_char) error stop
end program iso_c_binding_01
