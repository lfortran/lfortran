program readnan2

   real:: xnan
   character:: cni*4 = 'NAN '
   read(cni,*) xnan

   print *, 'NaN > NaN?', xnan > xnan
   if ((xnan > xnan) .neqv. .false.) error stop

   print *, 'NaN >= NaN?', xnan >= xnan
   if ((xnan >= xnan) .neqv. .false.) error stop

   print *, 'NaN < NaN?', xnan < xnan
   if ((xnan < xnan) .neqv. .false.) error stop

   print *, 'NaN <= NaN?', xnan <= xnan
   if ((xnan <= xnan) .neqv. .false.) error stop

   print *, 'NaN == NaN?', xnan == xnan
   if ((xnan == xnan) .neqv. .false.) error stop

   print *, 'NaN /= NaN?', xnan /= xnan
   if ((xnan /= xnan) .neqv. .true.) error stop

end program readnan2

