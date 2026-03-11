! recursive allocation + recursive free-ing
program finalization_03
      type t
            character(10) :: str
            type(t), allocatable :: t_internal
      end type t

      type(t) :: instance
      allocate(instance%t_internal)
      allocate(instance%t_internal%t_internal)
      allocate(instance%t_internal%t_internal%t_internal)
      allocate(instance%t_internal%t_internal%t_internal%t_internal)
end program 