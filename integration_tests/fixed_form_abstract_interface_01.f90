      module fixed_form_abstract_interface_01_mod
      implicit none
      ABSTRACT INTERFACE
      function unary_fn(x)
      real, intent(in) :: x
      real :: unary_fn
      end function
      subroutine callback(n)
      integer, intent(inout) :: n
      end subroutine
      END INTERFACE
      abstractinterface
      function int_fn(i)
      integer, intent(in) :: i
      integer :: int_fn
      end function
      endinterface
      contains
      real function apply(f, x)
      interface
      function f(x)
      real, intent(in) :: x
      real :: f
      end function
      end interface
      real, intent(in) :: x
      abstract  interface
      subroutine local_cb()
      end subroutine
      end interface
      apply = f(x)
      end function
      real function sq(x)
      real, intent(in) :: x
      sq = x*x
      end function
      end module

      program fixed_form_abstract_interface_01
      use fixed_form_abstract_interface_01_mod
      implicit none
      Abstract Interface
      subroutine prog_cb(i)
      integer, intent(in) :: i
      end subroutine
      End Interface
      real :: r
      r = apply(sq, 3.0)
      print *, r
      if (abs(r - 9.0) > 1e-6) error stop
      end program
