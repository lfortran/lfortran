module xx
   integer :: nx = 4
   integer :: ny = 1
   integer :: nz = 1
end module xx

subroutine a(cs)
   use xx
   integer, dimension(nx * ny - 1), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3])) error stop
end subroutine

subroutine b(cs)
   use xx
   integer, dimension(nx * ny), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4])) error stop
end subroutine

subroutine c(cs)
   use xx
   integer, dimension(2 + 3), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4, 5])) error stop
end subroutine

subroutine d(cs)
   use xx
   integer, dimension(nx - 1), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3])) error stop
end subroutine

subroutine e(cs)
   use xx
   integer, dimension(1 + nx * ny), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4, 5])) error stop
end subroutine

subroutine f(cs)
   use xx
   integer, dimension(nx * ny * nz), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4])) error stop
end subroutine

subroutine g(cs)
   use xx
   integer, dimension(1 + 2 + 3), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4, 5, 6])) error stop
end subroutine

subroutine h(cs)
   use xx
   integer, dimension(nx * ny + ny * nz), intent(in) :: cs
   print *, cs
   if (all(cs /= [1, 2, 3, 4, 5])) error stop
end subroutine

program main
   integer, dimension(8) :: cs
   cs = [1, 2, 3, 4, 5, 6, 7, 8]
   call a(cs)
   call b(cs)
   call c(cs)
   call d(cs)
   call f(cs)
   call g(cs)
   call h(cs)
end program
