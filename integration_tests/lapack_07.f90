c Minimal test case extracted from LAPACK spttrs.f
c Tests: if/else with array section passed to external subroutine
c This pattern causes a compiler crash (segfault in LLVM codegen)
c Compile with: lfortran --fixed-form --implicit-interface --legacy-array-sections
      subroutine test( n, nrhs, d, e, b, ldb )
      integer            ldb, n, nrhs
      real               b( ldb, * ), d( * ), e( * )
      integer            nb
      external           sptts2
      nb = 64
      if( nb.ge.nrhs ) then
         call sptts2( n, nrhs, d, e, b, ldb )
      else
         call sptts2( n, nrhs, d, e, b( 1, 1 ), ldb )
      end if
      return
      end

      subroutine sptts2( n, nrhs, d, e, b, ldb )
      integer n, nrhs, ldb
      real d(*), e(*), b(ldb, *)
      end

      program lapack_07
      print *, "PASS"
      end
