program associate_22
   implicit none

   type base
      real :: r
      character(len=:), allocatable :: s
      integer, pointer :: x(:)
   end type

   integer, pointer :: p_int_var(:), t_int_var(:)
   type(base), pointer :: p_base_var(:), t_base_var(:)

   allocate( p_int_var(0), t_int_var(0) )

   print*, associated( p_int_var, t_int_var )
   if ( associated( p_int_var, t_int_var ) ) error stop 
   nullify( p_int_var )

   allocate( p_base_var(0), t_base_var(0) )

   print*, associated( p_base_var, t_base_var )
   if ( associated( p_base_var, t_base_var ) ) error stop 
   nullify( p_base_var )
end program
