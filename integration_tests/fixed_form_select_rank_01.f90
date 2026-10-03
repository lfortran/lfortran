      subroutine set_first(a, j)
          integer :: a(..)
          integer, intent(inout) :: j
          select rank (a)
          rank (1)
              call store
     $            (j)
          end select
      end subroutine

      subroutine store(x)
          integer, intent(inout) :: x
          x = 42
      end subroutine

      program fixed_form_select_rank_27
          integer :: a(3), j
          interface
              subroutine set_first(a, j)
                  integer :: a(..)
                  integer, intent(inout) :: j
              end subroutine
              subroutine store(x)
                  integer, intent(inout) :: x
              end subroutine
          end interface
          j = 0
          call set_first(a, j)
          if (j /= 42) error stop
          print *, j
      end program
