module interface_11_m
   use, intrinsic :: iso_fortran_env, only : real64
   implicit none
   public :: get_args
   interface  get_args;  module  procedure  get_scalar_i;          end interface
   interface  get_args;  module  procedure  get_scalar_real;       end interface
   interface  get_args;  module  procedure  get_scalar_d;          end interface
   interface  get_args;  module  procedure  get_scalar_complex;    end interface
   interface  get_args;  module  procedure  get_scalar_logical;    end interface
   interface  get_args;  module  procedure  many_args;             end interface
contains
   !============================================================================
   subroutine many_args(n, g)
      implicit none
      character(len=*), intent(in)          :: n
      class(*), intent(out)                 :: g
         call get_generic(n, g)
      contains
         subroutine get_generic(name, generic)
         character(len=*), intent(in)  :: name
         class(*), intent(out)         :: generic
            select type(generic)
               type is (integer);                   call get_args(name, generic)
               type is (real);                      call get_args(name, generic)
               type is (real(kind=real64));         call get_args(name, generic)
               type is (logical);                   call get_args(name, generic)
               type is (complex);                   call get_args(name, generic)
               class default
                  stop 'unknown type in *get_generic*'
            end select
         end subroutine get_generic
   end subroutine many_args
   !============================================================================
   subroutine get_scalar_i(keyword, i)
   character(len=*), intent(in)   :: keyword
   integer, intent(out)           :: i
   end subroutine get_scalar_i
   !============================================================================
   subroutine get_scalar_real(keyword, r)
   character(len=*), intent(in)   :: keyword
   real, intent(out)              :: r
   end subroutine get_scalar_real
   !============================================================================
   subroutine get_scalar_d(keyword, d)
   character(len=*), intent(in)   :: keyword
   real(kind=real64)              :: d
   end subroutine get_scalar_d
   !============================================================================
   subroutine get_scalar_complex(keyword, x)
   character(len=*), intent(in)   :: keyword
   complex, intent(out)           :: x
   end subroutine get_scalar_complex
   !============================================================================
   subroutine get_scalar_logical(keyword, l)
   character(len=*), intent(in)   :: keyword
   logical                        :: l
   end subroutine get_scalar_logical
   !============================================================================
end module interface_11_m

program interface_11
    use interface_11_m
    implicit none
end program interface_11

