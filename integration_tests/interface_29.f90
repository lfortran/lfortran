module bench_mod
contains
   subroutine run(x)
      interface
         integer function f()
         end function f
      end interface
      integer :: x
      if (x /= 1) error stop
      print *, x
   end subroutine run
end module bench_mod

program main
   use bench_mod
   call run(1)
end program main
