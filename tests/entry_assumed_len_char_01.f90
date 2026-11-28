program entry_assumed_len_char_01
  implicit none
  interface
    function f(s)
      character :: s*(*), f*(*)
    end function
    function e(s)
      character :: s*(*), e*(*)
    end function
  end interface
  character(len=16) :: s
  s = 'abcdef'
  print *, f(s)
  print *, e(s)
end program

function f(s)
  implicit none
  character :: s*(*), e*(*), f*(*)
  f = s
  entry e(s)
  e = s
end function
