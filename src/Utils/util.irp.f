BEGIN_PROVIDER [ logical, all_utils ]
  implicit none
  BEGIN_DOC
  ! Dummy provider to provide all utils
  END_DOC
  ! Do not move this : it greps itself
  BEGIN_SHELL [ /bin/bash ]
  for i in $(grep  "BEGIN_PROVIDER"  $QPACKAGE_ROOT/src/Utils/*.irp.f  \
   | grep ',' | cut -d ',' -f 2 | cut -d ']' -f 1 | tail --lines=+3 )
  do
    if [[ ! -z $i ]]
    then
      echo PROVIDE $i
    fi
  done
  END_SHELL
  call trap_signals
  
END_PROVIDER

double precision function binom_func(i,j)
  implicit none
  BEGIN_DOC
  !.. math                       :: 
  !
  !  \frac{i!}{j!(i-j)!}
  !
  END_DOC
  integer,intent(in)             :: i,j
  double precision               :: fact, f
  integer, save                  :: ifirst
  double precision, save         :: memo(0:15,0:15)
  !DEC$ ATTRIBUTES ALIGN : $IRP_ALIGN :: memo
  integer                        :: k,l
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
  integer                        :: k,l
  double precision               :: fact, f
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
  BEGIN_DOC
  ! Compute 1st dimension such that it is aligned for vectorization.
  END_DOC
  integer                        :: n
  include 'include/constants.F'
  if (mod(n,SIMD_vector/4) /= 0) then
    align_double= n + SIMD_vector/4 - mod(n,SIMD_vector/4)
  else
    align_double= n
  endif
end


double precision function fact(n)
  implicit none
  BEGIN_DOC
  ! n!
  END_DOC
  integer                        :: n
  double precision, save         :: memo(1:100)
  integer, save                  :: memomax = 1
  
  if (n<=memomax) then
    if (n<2) then
      fact = 1.d0
    else
      fact = memo(n)
    endif
    return
  endif
  
  integer                        :: i
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
  ! 1/n!
  END_DOC
  integer                        :: i
  double precision               :: fact
  do i=1,size(fact_inv)
    fact_inv(i) = 1.d0/fact(i)
  enddo
END_PROVIDER

double precision function dble_fact(n) result(fact2)
  implicit none
  BEGIN_DOC
  ! n!!
  END_DOC
  integer                        :: n
  double precision, save         :: memo(1:100)
  integer, save                  :: memomax = 1
  
  ASSERT (iand(n,1) /= 0)
  if (n<=memomax) then
    if (n<3) then
      fact2 = 1.d0
    else
      fact2 = memo(n)
    endif
    return
  endif
  
  integer                        :: i
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
  BEGIN_DOC
  ! Write the last git commit in file iunit.
  END_DOC
  integer, intent(in)            :: iunit
  write(iunit,*) '----------------'
  write(iunit,*) 'Last git commit:'
  BEGIN_SHELL [ /bin/bash ]
  git log -1 2>/dev/null | sed "s/'//g"| sed "s/^/    write(iunit,*) '/g" | sed "s/$/'/g" || echo "Unknown"
  END_SHELL
  write(iunit,*) '----------------'
end

BEGIN_PROVIDER [ double precision, inv_int, (128) ]
  implicit none
  BEGIN_DOC
  ! 1/i
  END_DOC
  integer                        :: i
  do i=1,128
    inv_int(i) = 1.d0/dble(i)
  enddo
END_PROVIDER

subroutine wall_time(t)
  implicit none
  BEGIN_DOC
  ! The equivalent of cpu_time, but for the wall time.
  END_DOC
  double precision, intent(out)  :: t
  integer                        :: c
  integer, save                  :: rate = 0
  if (rate == 0) then
    CALL SYSTEM_CLOCK(count_rate=rate)
  endif
  CALL SYSTEM_CLOCK(count=c)
  t = dble(c)/dble(rate)
end

BEGIN_PROVIDER [ integer, nproc ]
  implicit none
  BEGIN_DOC
  ! Number of current OpenMP threads
  END_DOC
  
  integer                        :: omp_get_num_threads
  nproc = 1
  !$OMP PARALLEL
  !$OMP MASTER
  !$ nproc = omp_get_num_threads()
  !$OMP END MASTER
  !$OMP END PARALLEL
END_PROVIDER


double precision function u_dot_v(u,v,sze)
  implicit none
  BEGIN_DOC
  ! Compute <u|v>
  END_DOC
  integer, intent(in)            :: sze
  double precision, intent(in)   :: u(sze),v(sze)
  
  integer                        :: i,t1, t2, t3, t4
  
  ASSERT (sze > 0)
  t1 = 0
  t2 = sze/4
  t3 = t2+t2
  t4 = t3+t2
  u_dot_v = 0.d0
  do i=1,t2
    u_dot_v = u_dot_v + u(t1+i)*v(t1+i) + u(t2+i)*v(t2+i) +          &
        u(t3+i)*v(t3+i) + u(t4+i)*v(t4+i)
  enddo
  do i=t4+t2+1,sze
    u_dot_v = u_dot_v + u(i)*v(i)
  enddo
  
end

double precision function u_dot_u(u,sze)
  implicit none
  BEGIN_DOC
  ! Compute <u|u>
  END_DOC
  integer, intent(in)            :: sze
  double precision, intent(in)   :: u(sze)
  
  integer                        :: i
  integer                        :: t1, t2, t3, t4
  
  ASSERT (sze > 0)
  t1 = 0
  t2 = sze/4
  t3 = t2+t2
  t4 = t3+t2
  u_dot_u = 0.d0
! do i=1,t2
!   u_dot_u = u_dot_u + u(t1+i)*u(t1+i) + u(t2+i)*u(t2+i) +          &
!       u(t3+i)*u(t3+i) + u(t4+i)*u(t4+i)
! enddo
! do i=t4+t2+1,sze
!   u_dot_u = u_dot_u+u(i)*u(i)
! enddo
  
  do i=1,sze
    u_dot_u = u_dot_u + u(i)*u(i)
  enddo
  
end

subroutine normalize(u,sze)
  implicit none
  BEGIN_DOC
  ! Normalizes vector u
  ! u is expected to be aligned in memory.
  END_DOC
  integer, intent(in)            :: sze
  double precision, intent(inout):: u(sze)
  double precision               :: d
  double precision, external     :: u_dot_u
  integer                        :: i
  
  !DIR$ FORCEINLINE
  d = u_dot_u(u,sze)
  if (d /= 0.d0) then
    d = 1.d0/dsqrt( d )
  endif
  if (d /= 1.d0) then
    do i=1,sze
      u(i) = d*u(i)
    enddo
  endif
end






