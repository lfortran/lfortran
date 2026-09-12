program associate_56
    implicit none
    complex :: z = (1., 2.)
    complex :: zc(0:2) = [(1.,-1.), (2.,-2.), (3.,-3.)]
    character(:), allocatable :: str
  
    ! scalar complex parts: designators => variables
    associate (re => z%re, im => z%im)
      re = 10.
      im = 20.
    end associate
    print *, 'z after writes      =', z
    if (z%re /= 10.) error stop 'z%re /= 10.'
    if (z%im /= 20.) error stop 'z%im /= 20.'
  
    ! array complex part: like a strided section
    associate (r => zc%re)
      print *, 'is_contiguous(zc%re)=', is_contiguous(r)
      print *, 'lbound(zc%re), ubound(zc%re), size(zc%re) =', lbound(r), ubound(r), size(r)
      if (is_contiguous(r) .neqv. .false.) error stop 'is_contiguous(r) .neqv. .false.'
      if (lbound(r, 1) /= 1) error stop 'lbound(r, 1) /= 1'
      if (ubound(r, 1) /= 3) error stop 'ubound(r, 1) /= 3'
      if (size(r, 1) /= 3) error stop 'size(r, 1) /= 3'
      r = [7., 8., 9.]
    end associate
  
    associate (im => zc%im)
      print *, 'is_contiguous(zc%im)=', is_contiguous(im)
      print *, 'lbound(zc%im), ubound(zc%im), size(zc%im) =', lbound(im), ubound(im), size(im)
      if (is_contiguous(im) .neqv. .false.) error stop 'is_contiguous(im) .neqv. .false.'
      if (lbound(im, 1) /= 1) error stop 'lbound(im, 1) /= 1'
      if (ubound(im, 1) /= 3) error stop 'ubound(im, 1) /= 3'
      if (size(im, 1) /= 3) error stop 'size(im, 1) /= 3'
      im = [-7., -8., -9.]
    end associate
  
    print *, 'zc after write      =', zc
    if (any(zc%re /= [7., 8., 9.])) error stop 'zc%re /= [7., 8., 9.]'
    if (any(zc%im /= [-7., -8., -9.])) error stop 'zc%im /= [-7., -8., -9.]'
  
    print *, "Done"
  
    ! type parameter inquiries: expressions => values
    str = 'hello'
    associate (k => z%kind, l => str%len)
      print *, 'z%kind, str%len     =', k, l
      if (k /= 4) error stop 'z%kind /= kind(z)'
      if (l /= 5) error stop 'str%len /= len(str)'
    end associate
end program associate_56