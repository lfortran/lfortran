module derived_types_38_mod
   type :: m
      integer :: i
   end type m
end module derived_types_38_mod

program derived_types_38
   use derived_types_38_mod
   ! Uncomment below test case after supporting procedure
   ! declaration using subroutine, inside a derived type declared 
   ! in `program`.

   type :: t
      integer :: i
      !   procedure(f), nopass, pointer :: proc
   end type t

   type(m) :: m_var
   type(t) :: t_var

   m_var%i = 1
   t_var%i = 2

   call f()
   call g()

!    t_var%proc => g
!    call t_var%proc()

   print *, t_var%i
   if (t_var%i /= 3) error stop

contains

   subroutine f()
      print *, m_var
      if (m_var%i /= 1) error stop
   end subroutine f

   subroutine g()
      t_var%i = t_var%i + 1
   end subroutine g

end program derived_types_38
