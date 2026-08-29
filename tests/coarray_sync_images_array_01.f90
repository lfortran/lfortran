! `sync images` with an array image set makes the array-by-data pass transform
! the synthesized prif_sync_images interface, putting it in proc2newproc.
!
! Regression test: the pass then inspected the owner of every module-procedure
! interface it had transformed, assuming it was a Module. The coarray pass
! synthesizes the prif interfaces directly in the global scope, whose owner is
! the TranslationUnit rather than a symbol, so the unconditional down_cast to
! symbol_t tripped LCOMPILERS_ASSERT(is_a<T>(*f)) (Caffeine's
! app/native-multi-image.F90).
program coarray_sync_images_array_01
  implicit none
  integer :: img(2)
  img(1) = 1
  img(2) = 1
  sync images(img)
  print *, img(1)
end program coarray_sync_images_array_01
