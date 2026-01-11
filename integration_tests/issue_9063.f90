  module issue_9063_m
    implicit none

    type :: ctx_t
      procedure(f_iface), pointer, nopass :: fn => null()
    end type ctx_t

    abstract interface
      real function f_iface(x)
        real, intent(in) :: x
      end function f_iface
    end interface

  end module issue_9063_m

  program test_main
      use issue_9063_m
      print *, "Test Passed Successfully!"
  end program test_main

