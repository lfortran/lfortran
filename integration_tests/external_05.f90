subroutine idz_snorm(matvec)
   external matvec
   call matvec(n,v,m,u,p1,p2,p3,p4)
end

subroutine idz_diffsnorm0(matvec)

   external matvec

   call matvec(n,v,m,u1,p1,p2,p3,p4)

   return
end

program external_04
end program

