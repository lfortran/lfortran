      program fixed_form_select_case_02
          integer :: i, j
          i = 1
          j = 0
          select case (i)
          case (1)
              call sub
     $            (j)
          end select
          if (j /= 42) error stop
          print *, j
      contains
          subroutine sub(x)
              integer, intent(inout) :: x
              x = 42
          end subroutine sub
      end program
