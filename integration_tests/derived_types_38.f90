module derived_types_38_mod
   type :: m
      integer :: i
   end type m
end module derived_types_38_mod

program derived_types_38
   use derived_types_38_mod

   type :: t
      integer :: i
   end type t

   type(m) :: m_var
   type(t) :: t_var

   m_var%i = 1
   t_var%i = 2

   call f()

   print *, t_var
   if (t_var%i /= 3) error stop

contains

   subroutine f()
      print *, m_var
      if (m_var%i /= 1) error stop
      t_var%i = t_var%i + 1
   end subroutine f
end program derived_types_38
