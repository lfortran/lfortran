module internal_sub_shadows_generic_01_m1
  implicit none
  interface one
    module procedure one1
  end interface
contains
  subroutine one1()
    error stop 1
  end subroutine
end module

module internal_sub_shadows_generic_01_m2
  use internal_sub_shadows_generic_01_m1, only: one
  implicit none
contains
  subroutine two()
    call one()
  contains
    subroutine one()
      print *, 42
    end subroutine
  end subroutine
end module

program internal_sub_shadows_generic_01
  use internal_sub_shadows_generic_01_m2
  implicit none
  call two()
end program
