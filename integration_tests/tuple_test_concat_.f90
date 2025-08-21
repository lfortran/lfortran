program test_tuple_concat

   implicit none
   call tuple_concat()

contains
   subroutine tuple_concat()

      _lfortran_tuple(integer, integer)                                                 :: t1
      _lfortran_tuple(real, character(len=:), integer)                                  :: t2
      _lfortran_tuple(real)                                                             :: t3
      _lfortran_tuple(integer, integer, real, character(len=:), integer, real)          :: t4
      _lfortran_tuple(integer, real)                                                    :: t5
      _lfortran_tuple(integer, real, integer, real)                                     :: t6
      _lfortran_tuple(integer, real, integer, real, integer, real)                      :: t7
      _lfortran_tuple(integer, real, integer, real, integer, real, integer, real)       :: t8
      _lfortran_list(_lfortran_tuple(integer, real))                                    :: l1
      integer:: start, i
      _lfortran_tuple(_lfortran_tuple(_lfortran_tuple(integer, integer), _lfortran_tuple(integer, real)), _lfortran_tuple(real, character(len=:), integer)) :: t9

      t1 = _lfortran_tuple_constant(1, 2)
      t2 = _lfortran_tuple_constant(3.0, "abc", -10)
      t3 = _lfortran_tuple_constant(10.0)
      t4 = _lfortran_concat(t1, _lfortran_concat(t2, t3))

      if ( .not. _lfortran_eq(t4, _lfortran_tuple_constant(_lfortran_get_item(t1, 0), &
         _lfortran_get_item(t1, 1), _lfortran_get_item(t2, 0),_lfortran_get_item(t2, 1), &
         _lfortran_get_item(t2, 2),_lfortran_get_item(t3, 0))) ) error stop

      if ( .not. _lfortran_eq(_lfortran_concat(t4, t3), _lfortran_concat(t1, t2, t3, &
         _lfortran_tuple_constant(_lfortran_get_item(t3, 0)))) ) error stop

      start = 117
      do i = start, start + 2
         t5 = _lfortran_tuple_constant(i, real(i*i))
         call _lfortran_list_append(l1, t5)
         if (i == start) then
            t6 = _lfortran_concat(t5, _lfortran_get_item(l1, -1))
         else if (i == start + 1) then
            t7 = _lfortran_concat(t6, _lfortran_get_item(l1, -1))
         else
            t8 = _lfortran_concat(t7, _lfortran_get_item(l1, -1))
         end if
      end do

      if ( .not. _lfortran_eq(t6, _lfortran_concat(_lfortran_get_item(l1, 0), &
         _lfortran_get_item(l1, 0)))) error stop
      if ( .not. _lfortran_eq(t7, _lfortran_concat(t6, _lfortran_get_item(l1, 1)))) error stop
      if ( .not. _lfortran_eq(t8, _lfortran_concat(t7, _lfortran_get_item(l1, 2)))) error stop

      t9 = _lfortran_concat(_lfortran_tuple_constant(_lfortran_tuple_constant(t1, t5)), _lfortran_tuple_constant(t2))
      if ( .not. (_lfortran_eq(_lfortran_get_item(_lfortran_get_item(t9, 0), 0), t1) .and. &
         _lfortran_eq(_lfortran_get_item(_lfortran_get_item(t9, 0), 1), t5) .and. &
         _lfortran_eq(_lfortran_get_item(t9, 1), t2)) ) error stop
   end subroutine

end program test_tuple_concat

