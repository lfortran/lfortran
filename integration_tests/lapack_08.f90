c Minimal test case extracted from LAPACK strti2.f
c Tests: two loops with array sections passed to external subroutine
c Pattern: loop 1 uses A(1,J), loop 2 uses A(J+1,J+1) inside IF
c This causes compiler crash (segfault in LLVM codegen)
c Compile with: lfortran --fixed-form --implicit-interface --legacy-array-sections
      subroutine test( n, a, lda )
      integer            lda, n
      real               a( lda, * )
      integer            j
      external           foo
      do 10 j = 1, n
         call foo( n-j, a, lda, a( 1, j ), 1 )
   10 continue
      do 20 j = 1, n
         if( j.lt.n ) then
            call foo( n-j, a( j+1, j+1 ), lda, a( j+1, j ), 1 )
         end if
   20 continue
      return
      end

      subroutine foo( n, a, lda, x, incx )
      integer n, lda, incx
      real a(lda, *), x(*)
      end

      program lapack_08
      print *, "PASS"
      end
