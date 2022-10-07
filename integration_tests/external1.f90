program main
  external sub1    ! external statement
  integer, external :: fun1 ! external attribute
  integer,external :: fun2
  !call g(sub1,fun1)
end program

! TODO: this is valid Fortran code -- you can pass a function as an argument; we will need to figure out
!       how to do that.

!subroutine g(proc,fun)
!  external proc
!  integer :: fun,i
!  i=fun()
!  call proc(i)
!end subroutine

subroutine sub1(i)
  integer :: i
  write(*,*) i
end subroutine

integer function fun1()
    integer a
    !a = 100
    !a = a+1
end function

! TODO the below should work fine the moment LFortran implements statement functions
!integer function fun2()
!    integer a
!    a = 100
!    fun2 = a + 1
!end function
