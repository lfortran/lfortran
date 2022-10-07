program main
  integer a, b, c
  external f
  call f(a, b, c)
end program

subroutine f(x, y, z)
    double precision x, y, z
    print *, x, y, z
end subroutine
