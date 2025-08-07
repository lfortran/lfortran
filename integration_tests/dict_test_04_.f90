module dict_test_04_mod
   implicit none
contains
   subroutine test_dict()

      _lfortran_dict(_lfortran_tuple(integer, integer), integer(8)):: terms2poly
      _lfortran_dict(_lfortran_tuple(integer(8), integer(8)), _lfortran_tuple(real, real)):: rtheta2coords
      integer:: i, size, size1
      integer(8):: n
      real:: theta, r, eps, pi
      _lfortran_tuple(real, real):: coords

      eps = 1e-3
      pi = 3.1415
      size = 7000
      n = int(0, kind=8)

      do i = 1000, 1000+size-1, 7
         call _lfortran_set_item(terms2poly, _lfortran_tuple_constant(i, i*i), int(i + i*i, kind=8))

         theta = real(n) * pi
         r = real(i)
         call _lfortran_set_item(rtheta2coords, _lfortran_tuple_constant(int(i, kind=8), n), &
            _lfortran_tuple_constant(r * sin(theta), r * cos(theta)))

         n = n + int(1, kind=8)
      end do

      size1 = size/7
      n = int(0, kind=8)

      do i = 1000, 1000+size/2-1, 7
         if (_lfortran_pop(terms2poly, _lfortran_tuple_constant(i, i*i)) /= int(i + i*i, kind=8)) error stop
         theta = real(n) * pi
         r = real(i)
         coords = _lfortran_pop(rtheta2coords, _lfortran_tuple_constant(int(i, kind=8), n))
         if ( abs(_lfortran_get_item(coords, 0) - r*sin(theta)) > eps ) error stop
         if ( abs(_lfortran_get_item(coords, 1) - r*cos(theta)) > eps ) error stop

         size1 = size1 - 1
         if ( _lfortran_len(terms2poly) /= size1 ) error stop

         n = n + int(1, kind=8)
      end do

      n = int(0, kind=8)
      do i = 1000, 1000+size/2-1, 7
         call _lfortran_set_item(terms2poly, _lfortran_tuple_constant(i, i*i), int(1+ 2*i + i*i, kind=8))

         theta = real(n) * pi
         r = real(i)
         call _lfortran_set_item(rtheta2coords, _lfortran_tuple_constant(int(i, kind=8), n), &
            _lfortran_tuple_constant(r * cos(theta), r * sin(theta)))

         n = n + int(1, kind=8)
      end do

      n = int(0, kind=8)
      do i = 1000, 1000+size/2-1, 7
         if (_lfortran_get_item(terms2poly, _lfortran_tuple_constant(i, i*i)) /= int(1 + 2*i + i*i, kind=8)) error stop
         theta = real(n) * pi
         r = real(i)
         coords = _lfortran_pop(rtheta2coords, _lfortran_tuple_constant(int(i, kind=8), n))
         if ( abs(_lfortran_get_item(coords, 0) - r*cos(theta)) > eps ) error stop
         if ( abs(_lfortran_get_item(coords, 1) - r*sin(theta)) > eps ) error stop

         n = n + int(1, kind=8)
      end do

      n = int(0, kind=8)
      do i = 1000, 1000+size-1, 7
         call _lfortran_set_item(terms2poly, _lfortran_tuple_constant(i, i*i), int(1+ 2*i + i*i, kind=8))

         theta = real(n) * pi
         r = real(i)
         call _lfortran_set_item(rtheta2coords, _lfortran_tuple_constant(int(i, kind=8), n), &
            _lfortran_tuple_constant(r * cos(theta), r * sin(theta)))

         n = n + int(1, kind=8)
      end do

      n = int(0, kind=8)
      do i = 1000, 1000+size-1, 7
         if (_lfortran_get_item(terms2poly, _lfortran_tuple_constant(i, i*i)) /= int((1 + i)*(1 + i), kind=8)) error stop
         theta = real(n) * pi
         r = real(i)
         n = n + int(1, kind=8)
      end do
   end subroutine

end module


program test_dict_main
   use dict_test_04_mod
   call test_dict()
end program
