double precision function binom_func(i,j)
 implicit none
 integer,intent(in) :: i,j
 double precision :: fact, f
 integer, save :: ifirst
 double precision, save :: memo(0:15,0:15)
 !DEC$ ATTRIBUTES ALIGN : $IRP_ALIGN :: memo
 integer :: k,l
 if (ifirst == 0) then
   ifirst = 1
   do k=0,15
    f = fact(k)
    do l=0,15
     memo(k,l) = f/(fact(l)*fact(k-l))
    enddo
   enddo
 endif
 if ( (i<=15).and.(j<=15) ) then
   binom_func = memo(i,j)
 else
   binom_func = fact(i)/(fact(j)*fact(i-j))
 endif
end




 BEGIN_PROVIDER [ double precision, binom, (0:20,0:20) ]
&BEGIN_PROVIDER [ double precision, binom_transp, (0:20,0:20) ]
 implicit none
 BEGIN_DOC
! Binomial coefficients
 END_DOC
 integer :: k,l
 double precision :: fact, f
 do k=0,20
  f = fact(k)
  do l=0,20
   binom(k,l) = f/(fact(l)*fact(k-l))
   binom_transp(l,k) = binom(k,l)
  enddo
 enddo
END_PROVIDER


integer function align_double(n)
 implicit none
 integer :: n
 include 'include/constants.F'
 if (mod(n,SIMD_vector/4) /= 0) then
   align_double= n + SIMD_vector/4 - mod(n,SIMD_vector/4)
 else
   align_double= n
 endif
end


double precision function fact(n)
  implicit none
  integer :: n
  double precision, save :: memo(1:100)
  integer, save :: memomax = 1

  if (n<=memomax) then
    if (n<2) then
      fact = 1.d0
    else
      fact = memo(n)
    endif
    return
  endif

  integer :: i
  memo(1) = 1.d0
  do i=memomax+1,min(n,100)
    memo(i) = memo(i-1)*float(i)
  enddo
  memomax = min(n,100)
  fact = memo(memomax)
  do i=101,n
    fact = fact*float(i)
  enddo
end function



BEGIN_PROVIDER [ double precision, fact_inv, (128) ]
 implicit none
 BEGIN_DOC
! 1.d0/fact(k)
 END_DOC
 integer :: i
 double precision :: fact
  do i=1,size(fact_inv)
    fact_inv(i) = 1.d0/fact(i)
  enddo
END_PROVIDER

double precision function dble_fact(n) result(fact2)
  implicit none
  integer :: n
  double precision, save :: memo(1:100)
  integer, save :: memomax = 1

  ASSERT (iand(n,1) /= 0)
  if (n<=memomax) then
    if (n<3) then
      fact2 = 1.d0
    else
      fact2 = memo(n)
    endif
    return
  endif

  integer :: i
  memo(1) = 1.d0
  do i=memomax+2,min(n,99),2
    memo(i) = memo(i-2)* float(i)
  enddo
  memomax = min(n,99)
  fact2 = memo(memomax)

  do i=101,n,2
    fact2 = fact2*float(i)
  enddo

end function

subroutine write_git_log(iunit)
 implicit none
 integer, intent(in) :: iunit
 write(iunit,*) '----------------'
 write(iunit,*) 'Last git commit:'
 BEGIN_SHELL [ /bin/bash ]
   git log -1 | sed "s/'//g"| sed "s/^/    write(iunit,*) '/g" | sed "s/$/'/g"
 END_SHELL
 write(iunit,*) '----------------'
end

BEGIN_PROVIDER [ double precision, inv_int, (128) ]
 implicit none
 BEGIN_DOC
! 1/i
 END_DOC
 integer :: i
 do i=1,size(inv_int)
   inv_int(i) = 1.d0/dble(i)
 enddo

END_PROVIDER

subroutine wall_time(t)
  implicit none
  double precision, intent(out) :: t
  integer :: c
  integer, save :: rate = 0
  if (rate == 0) then
    CALL SYSTEM_CLOCK(count_rate=rate)
  endif
  CALL SYSTEM_CLOCK(count=c)
  t = dble(c)/dble(rate)
end

BEGIN_PROVIDER [ integer, nproc ]
 implicit none
 BEGIN_DOC
! Number of current openmp threads
 END_DOC

 integer :: omp_get_num_threads
 nproc = 1
 !$OMP PARALLEL
 !$OMP MASTER
 !$ nproc = omp_get_num_threads()
 !$OMP END MASTER
 !$OMP END PARALLEL
END_PROVIDER

