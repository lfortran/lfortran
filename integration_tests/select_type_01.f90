program select_type_01
implicit none
  type base
    integer::i
  end type

  type, extends(base)::child
    integer::j
  end type

  class(base), pointer :: bptr

  type(base), target :: base_target = base(10)
  type(child), target :: child_target

  child_target = child(20, 30)

  print *, base_target%i
  print *, child_target%i, child_target%j

  bptr => child_target

  call perform_select_type(bptr)

  bptr => base_target

  call perform_select_type(bptr)

contains

  subroutine perform_select_type(bptr)
    class(base), pointer :: bptr
    select type(bptr)
      type is (base)
      print *, "base type: component value: ", bptr%i
      if( bptr%i /= 10 ) error stop
      type is (child)
      print *, "child type: component values: ", bptr%i, bptr%j
      if( bptr%i /= 20 ) error stop
      if( bptr%j /= 30 ) error stop
    end select
  end subroutine

end program select_type_01
