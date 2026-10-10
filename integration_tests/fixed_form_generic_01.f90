      module fixed_form_generic_01_mod
      implicit none
      interface gen
      module procedure gen_int, gen_real
      module procedure :: gen_log
      end interface
      interface other
      module procedure gen_int
      end interface other
      contains
      integer function gen_int(x)
      integer, intent(in) :: x
      gen_int = 2*x
      end function
      real function gen_real(x)
      real, intent(in) :: x
      gen_real = 3*x
      end function
      integer function gen_log(x)
      logical, intent(in) :: x
      gen_log = 0
      if (x) gen_log = 1
      end function
      end module

      program fixed_form_generic_01
      use fixed_form_generic_01_mod
      implicit none
      print *, gen(4), gen(1.5), gen(.true.), other(5)
      if (gen(4) /= 8) error stop
      if (abs(gen(1.5) - 4.5) > 1e-6) error stop
      if (gen(.true.) /= 1) error stop
      if (other(5) /= 10) error stop
      end program
