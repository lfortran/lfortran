program main
  implicit none

  integer, pointer :: t(:)
  integer, pointer :: u(:)

  allocate(t(5))
  t = 1
  u(0:5) => t !! Should raise runtime error 
  u => t(0:) !! Should raise runtime error 
  print *, u
end program main