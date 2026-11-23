module m_vector_subscript_04
   implicit none
contains
   function twice(arr) result(r)
      integer, intent(in) :: arr(:)
      integer :: r(size(arr))
      r = 2*arr
   end function twice
end module m_vector_subscript_04

program vector_subscript_04
   use m_vector_subscript_04
   implicit none
   integer :: b(4) = [10, 20, 30, 40]
   integer :: k(4) = [4, 3, 2, 1]
   integer :: t(4, 1) = reshape([1, 2, 3, 4], [4, 1])
   integer :: r(4)
   ! Nested vector subscript as a function actual: b(k(t(:,1))) is [40, 30, 20, 10].
   r = twice(b(k(t(:, 1))))
   if (any(r /= [80, 60, 40, 20])) error stop
end program vector_subscript_04
