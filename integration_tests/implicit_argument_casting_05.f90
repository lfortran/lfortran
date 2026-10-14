      module casting_array_mod
      double precision mem(10), same_mem(10)
      end module
      subroutine casting_array_caller
      use casting_array_mod
      call callee(mem(1))
      end
      subroutine callee(a)
      integer a(10)
      a(1)=1
      end
      subroutine same_type_callee(a)
      double precision a(10)
      a(1)=42d0
      end
      program implicit_argument_casting_05
      use casting_array_mod
      mem=0d0
      call casting_array_caller
      same_mem=0d0
      call same_type_callee(same_mem(1))
      if (same_mem(1).ne.42d0) stop 1
      end
