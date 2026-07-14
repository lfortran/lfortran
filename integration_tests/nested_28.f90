module nested_28_mod
contains

  character(4) function g(k)
    integer :: k

    g = f(k)

  contains

    function f(n)
      character(3), parameter :: names(1) = ["abc"]
      integer :: n
      character(len_trim(names(n))) :: f

      f = "abc"
    end function f

  end function g

end module nested_28_mod

program nested_28
  use nested_28_mod
  print *, g(1)
  if (len_trim(g(1)) /= 3) error stop
end program nested_28
