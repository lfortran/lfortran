program array_constructor_05
  implicit none
  type :: vertex_type
    integer :: id = -1
  end type
  type :: graph_type
    type(vertex_type), dimension(:), allocatable :: vertex
  end type
  type(graph_type) :: g
  integer :: idx, arr(5)
  allocate(g%vertex(5))
  g%vertex(1)%id = 10
  g%vertex(2)%id = 20
  g%vertex(3)%id = 30
  g%vertex(4)%id = 40
  g%vertex(5)%id = 50
  arr = [g%vertex(:)%id]
  if (arr(1) /= 10) error stop
  if (arr(3) /= 30) error stop
  if (arr(5) /= 50) error stop
  idx = findloc([g%vertex(:)%id], 30, dim=1)
  if (idx /= 3) error stop
  print *, "PASS"
end program
