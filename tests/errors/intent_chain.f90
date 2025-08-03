program test_intent_chain
  implicit none
  integer :: x = 10

  print *, "In main, before call, x =", x

  call sub_a(x)

  print *, "In main, after call,  x =", x

contains
  subroutine sub_a(val_a)
    integer, intent(in) :: val_a

    ! This call is illegal because sub_b expects to modify its argument,
    ! but val_a is a read-only intent(in) variable.
    call sub_b(val_a)

    print *, "In sub_a, after call, val_a =", val_a
  end subroutine sub_a

  subroutine sub_b(val_b)
    integer, intent(inout) :: val_b
    val_b = 20 ! Modify the argument
  end subroutine sub_b
end program test_intent_chain
