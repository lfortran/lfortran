! Test same_type_as between a polymorphic object and a non-polymorphic type
program class_152
  implicit none

  type :: base
    integer :: i = 0
  end type
  type, extends(base) :: child
    real :: r = 0.
  end type

  class(base), allocatable :: obj
  type(child) :: probe
  type(base) :: base_probe

  allocate(child :: obj)
  base_probe = base(i=1)
  associate (o => obj)
    print *, same_type_as(o, probe)
    print *, same_type_as(o, base_probe)
    if (.not. same_type_as(o, probe)) error stop
    if (same_type_as(o, base_probe)) error stop
    select type (o)                        ! only legal if o is polymorphic
    type is (child)
      print *, 'select type sees CHILD'

    class default
      print *, 'select type sees base'
      error stop
    end select
  end associate

  print *, "Done"
end program class_152
