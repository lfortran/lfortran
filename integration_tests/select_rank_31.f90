module select_rank_31_mod
    implicit none
    type :: array_type
      real, allocatable, dimension(:,:) :: val
    end type
    type :: array_ptr_type
      type(array_type), allocatable, dimension(:,:) :: array
    end type
  contains
    subroutine save_input(input, kind_)
      class(*), dimension(..), intent(in) :: input
      integer, intent(out) :: kind_
      kind_ = 0
      select rank(input)
      rank(0)
        select type(input)
        class is(array_type)
          kind_ = 1
        type is(array_ptr_type)
          kind_ = 2
        end select
      rank default
        kind_ = -1
      end select
    end subroutine save_input
  end module select_rank_31_mod
  
  program select_rank_31
    use select_rank_31_mod
    implicit none
    type(array_type) :: scalar_arr
    integer :: kind_
    integer, dimension(2,3) :: arr2
    call save_input(arr2, kind_)
    if (kind_ /= -1) error stop
  
    print *, "PASS"
end program select_rank_31