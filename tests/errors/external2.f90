program main
  integer a, b, c
  external f
  call f(a, b, c)
end program

subroutine f(x)
    double precision x
    print *, x
end subroutine
