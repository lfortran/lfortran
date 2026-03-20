  module proc_ptr_nopass_01_m
    implicit none

    type :: ctx_t
      procedure(f_iface), pointer, nopass :: fn => null()
    end type ctx_t

    abstract interface
      real function f_iface(x)
        real, intent(in) :: x
      end function f_iface
    end interface

  end module proc_ptr_nopass_01_m

  program proc_ptr_nopass_01
      use proc_ptr_nopass_01_m
      print *, "Test Passed Successfully!"
  end program proc_ptr_nopass_01

