module array_section_28_mod
    implicit none
    type :: graph_type
      integer, allocatable :: adjacency(:,:)
    end type
  contains
    subroutine run(graph, parent_vertices, child_vertices, j, do_it)
      type(graph_type), intent(in) :: graph
      integer, allocatable, intent(in) :: parent_vertices(:)
      integer, allocatable, intent(in) :: child_vertices(:)
      integer, intent(in) :: j
      logical, intent(in) :: do_it
      if (do_it) then
        call consume([graph%adjacency(parent_vertices(:), child_vertices(j))])
      end if
    end subroutine
    subroutine consume(arr)
      integer, intent(in) :: arr(:)
      if (size(arr) < 1) error stop
    end subroutine
  end module
  
  program array_section_28
    use array_section_28_mod
    implicit none
    type(graph_type) :: g
    integer, allocatable :: pv(:), cv(:)
    allocate(g%adjacency(3,3))
    allocate(pv(3)); pv = [1,2,3]
    allocate(cv(3)); cv = [1,2,3]
    g%adjacency = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    call run(g, pv, cv, 2, .false.)
end program array_section_28