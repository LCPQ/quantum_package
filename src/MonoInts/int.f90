!! Vps= <Phi_A|Vloc(C)+Cpp(C)| Phi_B>
!!
!! with: Vloc(C)=\sum_{k=1}^klocmax v_k rC**n_k exp(-dz_k rC**2)
!!       Vpp(C)=\sum_{l=0}^lmax\sum_{k=1}^kmax v_kl rC**n_kl exp(-dz_kl rC**2)*|l><l|
double precision function Vps &
(a,n_a,g_a,b,n_b,g_b,c,klocmax,v_k,n_k,dz_k,lmax,kmax,v_kl,n_kl,dz_kl)
implicit none
integer n_a(3),n_b(3)
double precision g_a,g_b,a(3),b(3),c(3)
integer kmax_max,lmax_max
parameter (kmax_max=2,lmax_max=2)
integer lmax,kmax,n_kl(kmax_max,0:lmax_max)
double precision v_kl(kmax_max,0:lmax_max),dz_kl(kmax_max,0:lmax_max)
integer klocmax_max
parameter (klocmax_max=10)
integer klocmax,n_k(klocmax_max)
double precision v_k(klocmax_max),dz_k(klocmax_max)
double precision Vloc,Vpseudo

Vps=Vloc(klocmax,v_k,n_k,dz_k,a,n_a,g_a,b,n_b,g_b,c) &
   +Vpseudo(lmax,kmax,v_kl,n_kl,dz_kl,a,n_a,g_a,b,n_b,g_b,c)
end
!!
!! Vps_num: brute force numerical evaluation of the same matrix element Vps
!!
double precision function Vps_num &
(npts,rmax,a,n_a,g_a,b,n_b,g_b,c,klocmax,v_k,n_k,dz_k,lmax,kmax,v_kl,n_kl,dz_kl)
implicit none
integer n_a(3),n_b(3)
double precision g_a,g_b,a(3),b(3),c(3),rmax
integer kmax_max,lmax_max
parameter (kmax_max=2,lmax_max=2)
integer lmax,kmax,n_kl(kmax_max,0:lmax_max)
double precision v_kl(kmax_max,0:lmax_max),dz_kl(kmax_max,0:lmax_max)
integer klocmax_max;parameter (klocmax_max=10)
integer klocmax,n_k(klocmax_max)
double precision v_k(klocmax_max),dz_k(klocmax_max)
double precision Vloc_num,Vpseudo_num,v1,v2
integer npts,nptsgrid
nptsgrid=50
call initpseudos(nptsgrid)
v1=Vloc_num(npts,rmax,klocmax,v_k,n_k,dz_k,a,n_a,g_a,b,n_b,g_b,c) 
v2=Vpseudo_num(nptsgrid,rmax,lmax,kmax,v_kl,n_kl,dz_kl,a,n_a,g_a,b,n_b,g_b,c)
Vps_num=v1+v2
end

double precision function Vloc_num(npts_over,xmax,klocmax,v_k,n_k,dz_k,a,n_a,g_a,b,n_b,g_b,c)
implicit none
integer klocmax_max
parameter (klocmax_max=10)
integer klocmax
double precision v_k(klocmax_max),dz_k(klocmax_max)
integer n_k(klocmax_max)
integer npts_over,ix,iy,iz
double precision xmax,dx,x,y,z
double precision a(3),b(3),c(3),term,r,orb_phi,g_a,g_b,ac(3),bc(3)
integer n_a(3),n_b(3),k,l
do l=1,3
 ac(l)=a(l)-c(l)
 bc(l)=b(l)-c(l)
enddo
dx=2.d0*xmax/npts_over
Vloc_num=0.d0
do ix=1,npts_over
do iy=1,npts_over
do iz=1,npts_over
 x=-xmax+dx*ix+dx/2.d0
 y=-xmax+dx*iy+dx/2.d0
 z=-xmax+dx*iz+dx/2.d0
 term=orb_phi(x,y,z,n_a,ac,g_a)*orb_phi(x,y,z,n_b,bc,g_b)
 r=dsqrt(x**2+y**2+z**2)
 do k=1,klocmax 
  Vloc_num=Vloc_num+dx**3*v_k(k)*r**n_k(k)*dexp(-dz_k(k)*r**2)*term
 enddo
enddo
enddo
enddo
end

double precision function orb_phi(x,y,z,npower,center,gamma)
implicit none
integer npower(3)
double precision x,y,z,r2,gamma,center(3)
r2=(x-center(1))**2+(y-center(2))**2+(z-center(3))**2
orb_phi=(x-center(1))**npower(1)*(y-center(2))**npower(2)*(z-center(3))**npower(3)
orb_phi=orb_phi*dexp(-gamma*r2)
end

!!  Real spherical harmonics  Ylm  

!  factor =  ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2
!  Y_lm(theta,phi) = 
!   m > 0 factor* P_l^|m|(cos(theta)) cos (|m| phi)
!   m = 0 1/sqrt(2) *factor* P_l^0(cos(theta)) 
!   m < 0 factor* P_l^|m|(cos(theta)) sin (|m| phi)
!
! x=cos(theta)

        double precision function ylm_real(l,m,x,phi)
        implicit double precision (a-h,o-z)
        DIMENSION PM(0:100,0:100)
        MM=100
        pi=dacos(-1.d0)
        iabs_m=iabs(m)
        if(iabs_m.gt.l)stop 'm must be between -l and l'
        factor= dsqrt( ((2*l+1)*fact(l-iabs_m))/(4.d0*pi*fact(l+iabs_m)) )
        if(dabs(x).gt.1.d0)then
         print*,'pb. in ylm_no'
         print*,'x=',x
         stop 
        endif
        call LPMN(MM,l,l,X,PM)
        plm=PM(iabs_m,l)
        coef=factor*plm
        if(m.gt.0)ylm_real=dsqrt(2.d0)*coef*dcos(iabs_m*phi)
        if(m.eq.0)ylm_real=coef
        if(m.lt.0)ylm_real=dsqrt(2.d0)*coef*dsin(iabs_m*phi)

         fourpi=4.d0*dacos(-1.d0)
         if(l.eq.0)ylm_real=dsqrt(1.d0/fourpi)

         xchap=dsqrt(1.d0-x**2)*dcos(phi)
         ychap=dsqrt(1.d0-x**2)*dsin(phi)
         zchap=x
         if(l.eq.1.and.m.eq.1)ylm_real=dsqrt(3.d0/fourpi)*xchap
         if(l.eq.1.and.m.eq.0)ylm_real=dsqrt(3.d0/fourpi)*zchap
         if(l.eq.1.and.m.eq.-1)ylm_real=dsqrt(3.d0/fourpi)*ychap

         if(l.eq.2.and.m.eq.2)ylm_real=dsqrt(15.d0/16.d0/pi)*(xchap**2-ychap**2)
         if(l.eq.2.and.m.eq.1)ylm_real=dsqrt(15.d0/fourpi)*xchap*zchap
         if(l.eq.2.and.m.eq.0)ylm_real=dsqrt(5.d0/16.d0/pi)*(-xchap**2-ychap**2+2.d0*zchap**2)
         if(l.eq.2.and.m.eq.-1)ylm_real=dsqrt(15.d0/fourpi)*ychap*zchap
         if(l.eq.2.and.m.eq.-2)ylm_real=dsqrt(15.d0/fourpi)*xchap*ychap

         if(l.gt.2)stop 'l > 2 not coded!'

        end
!                                   _       
!                                  | |      
! __   __  _ __  ___  ___ _   _  __| | ___  
! \ \ / / | '_ \/ __|/ _ \ | | |/ _` |/ _ \ 
!  \ V /  | |_) \__ \  __/ |_| | (_| | (_) |
!   \_/   | .__/|___/\___|\__,_|\__,_|\___/ 
!         | |                               
!         |_|                               

!! Routine Vpseudo is based on formumla (66) 
!! of Kahn Baybutt TRuhlar J.Chem.Phys. vol.65 3826 (1976):
!!
!! Vpseudo= (4pi)**2* \sum_{l=0}^lmax \sum_{m=-l}^{l}
!! \sum{lambda=0}^{l+nA} \sum_{mu=-lambda}^{lambda}
!! \sum{k1=0}^{nAx} \sum{k2=0}^{nAy} \sum{k3=0}^{nAz}
!! binom(nAx,k1)*binom(nAy,k2)*binom(nAz,k3)* Y_{lambda mu}(AC_unit)
!! *CAx**(nAx-k1)*CAy**(nAy-k2)*CAz**(nAz-k3)*
!! bigI(lambda,mu,l,m,k1,k2,k3)
!! \sum{lambdap=0}^{l+nB} \sum_{mup=-lambdap}^{lambdap}
!! \sum{k1p=0}^{nBx} \sum{k2p=0}^{nBy} \sum{k3p=0}^{nBz}
!! binom(nBx,k1p)*binom(nBy,k2p)*binom(nBz,k3p)* Y_{lambdap mup}(BC_unit)
!! *CBx**(nBx-k1p)*CBy**(nBy-k2p)*CBz**(nBz-k3p)*
!! bigI(lambdap,mup,l,m,k1p,k2p,k3p)*
!! \sum_{k=1}^{kmax} v_kl(k,l)*
!! bigR(lambda,lambdap,k1+k2+k3+k1p+k2p+k3p+n_kl(k,l),g_a,g_b,AC,BC,dz_kl(k,l))
!!
!! nA=nAx+nAy+nAz
!! nB=nBx+nBy+nBz
!! AC=|A-C|
!! AC_x= A_x-C_x, etc.
!! BC=|B-C|
!! AC_unit= vect(AC)/AC
!! BC_unit= vect(BC)/BC
!! bigI(lambda,mu,l,m,k1,k2,k3)= 
!! \int dOmega Y_{lambda mu}(Omega) xchap^k1 ychap^k2 zchap^k3  Y_{l m}(Omega) 
!!
!! bigR(lambda,lambdap,N,g_a,g_b,gamm_k,AC,BC)
!! = exp(-g_a* AC**2 -g_b* BC**2) * int_prod_bessel_loc(ktot+2,g_a+g_b+dz_k(k),l,dreal)
!!   /int dx x^{ktot} exp(-g_k)*x**2) M_lambda(2 g_k D x)

double precision function Vpseudo  &
(lmax,kmax,v_kl,n_kl,dz_kl,a,n_a,g_a,b,n_b,g_b,c)
implicit none

! ___                
!  |  ._  ._     _|_ 
! _|_ | | |_) |_| |_ 
!         |          
double precision, intent(in) :: a(3),g_a,b(3),g_b,c(3)

integer kmax_max,lmax_max,ntot_max,nkl_max
parameter (kmax_max=2,lmax_max=2,nkl_max=4)
parameter (ntot_max=10)
integer, intent(in) :: lmax,kmax,n_kl(kmax_max,0:lmax_max)
integer, intent(in) :: n_a(3),n_b(3)
double precision, intent(in) :: v_kl(kmax_max,0:lmax_max),dz_kl(kmax_max,0:lmax_max)


!                     
! |   _   _  _. |  _  
! |_ (_) (_ (_| | (/_ 
!                     

double precision :: fourpi,f,prod,prodp,binom,accu,bigR,bigI,ylm
double precision :: theta_AC0,phi_AC0,theta_BC0,phi_BC0,ac,bc,big
double precision :: areal,freal,breal,t1,t2,int_prod_bessel
double precision :: arg

integer :: ntot,ntotA,m,mu,mup,k1,k2,k3,ntotB,k1p,k2p,k3p,lambda,lambdap,ktot
integer :: l,k

!  _                          
! |_) o  _     _. ._ ._ _.    
! |_) | (_|   (_| |  | (_| \/ 
!        _|                /  

double precision array_coefs_A(0:ntot_max,0:ntot_max,0:ntot_max)
double precision array_coefs_B(0:ntot_max,0:ntot_max,0:ntot_max)

double precision, allocatable :: array_R(:,:,:,:,:)
double precision, allocatable :: array_I_A(:,:,:,:,:)
double precision, allocatable :: array_I_B(:,:,:,:,:)

!=!=!=!=!=!=!=!=!=!
! A l l o c a t e !
!=!=!=!=!=!=!=!=!=!

allocate (array_R(0:ntot_max+nkl_max,kmax_max,0:lmax_max,0:lmax_max+ntot_max,0:lmax_max+ntot_max))

allocate (array_I_A(0:lmax_max+ntot_max,-(lmax_max+ntot_max):lmax_max+ntot_max,0:ntot_max,0:ntot_max,0:ntot_max))

allocate (array_I_B(0:lmax_max+ntot_max,-(lmax_max+ntot_max):lmax_max+ntot_max,0:ntot_max,0:ntot_max,0:ntot_max))

!  _                
! /   _. |  _     | 
! \_ (_| | (_ |_| | 
!                   

fourpi=4.d0*dacos(-1.d0)
ac=dsqrt((a(1)-c(1))**2+(a(2)-c(2))**2+(a(3)-c(3))**2)
bc=dsqrt((b(1)-c(1))**2+(b(2)-c(2))**2+(b(3)-c(3))**2)
arg=g_a*ac**2+g_b*bc**2
if(arg.gt.-dlog(10.d-20))then
Vpseudo=0.d0
return
endif
freal=dexp(-arg)

areal=2.d0*g_a*ac
breal=2.d0*g_b*bc
ntotA=n_a(1)+n_a(2)+n_a(3)
ntotB=n_b(1)+n_b(2)+n_b(3)
ntot=ntotA+ntotB

if(ntot.gt.ntot_max)stop 'increase ntot_max'

if(ac.eq.0.d0.and.bc.eq.0.d0)then


 !=!=!=!=!=!
 ! I n i t !
 !=!=!=!=!=!

 accu=0.d0

 !=!=!=!=!=!=!=!
 ! c a l c u l !
 !=!=!=!=!=!=!=!

 do k=1,kmax
  do l=0,lmax
   ktot=ntot+n_kl(k,l)
   do m=-l,l
    prod=bigI(0,0,l,m,n_a(1),n_a(2),n_a(3))
    prodp=bigI(0,0,l,m,n_b(1),n_b(2),n_b(3))
    accu=accu+prod*prodp*v_kl(k,l)*freal*int_prod_bessel(ktot+2,g_a+g_b+dz_kl(k,l),0,0,areal,breal)
   enddo
  enddo
 enddo

 !=!=!=!=!
 ! E n d !
 !=!=!=!=!

 Vpseudo=accu*fourpi

else if(ac.ne.0.d0.and.bc.ne.0.d0)then

 !=!=!=!=!=!
 ! I n i t !
 !=!=!=!=!=!

 f=fourpi**2

 theta_AC0=dacos( (a(3)-c(3))/ac )
 phi_AC0=datan2((a(2)-c(2))/ac,(a(1)-c(1))/ac)
 theta_BC0=dacos( (b(3)-c(3))/bc )
 phi_BC0=datan2((b(2)-c(2))/bc,(b(1)-c(1))/bc)




 do ktot=0,ntotA+ntotB+nkl_max
   do lambda=0,lmax+ntotA
   do lambdap=0,lmax+ntotB
        do k=1,kmax
          do l=0,lmax
          array_R(ktot,k,l,lambda,lambdap)= freal & 
                                            *int_prod_bessel(ktot+2,g_a+g_b+dz_kl(k,l),lambda,lambdap,areal,breal)
          enddo
        enddo
   enddo
   enddo
 enddo

 do k1=0,n_a(1)
 do k2=0,n_a(2)
 do k3=0,n_a(3)
  array_coefs_A(k1,k2,k3)=binom(n_a(1),k1)*binom(n_a(2),k2)*binom(n_a(3),k3)  &
                          *(c(1)-a(1))**(n_a(1)-k1)*(c(2)-a(2))**(n_a(2)-k2)*(c(3)-a(3))**(n_a(3)-k3)
 enddo
 enddo
 enddo

 do k1p=0,n_b(1)
 do k2p=0,n_b(2)
 do k3p=0,n_b(3)
  array_coefs_B(k1p,k2p,k3p)=binom(n_b(1),k1p)*binom(n_b(2),k2p)*binom(n_b(3),k3p)  &
                             *(c(1)-b(1))**(n_b(1)-k1p)*(c(2)-b(2))**(n_b(2)-k2p)*(c(3)-b(3))**(n_b(3)-k3p)
 enddo
 enddo
 enddo

 !=!=!=!=!=!=!=!
 ! c a l c u l !
 !=!=!=!=!=!=!=!

 accu=0.d0
 do l=0,lmax
  do m=-l,l

       do lambda=0,l+ntotA
        do mu=-lambda,lambda
           do k1=0,n_a(1)
           do k2=0,n_a(2)
           do k3=0,n_a(3)
                array_I_A(lambda,mu,k1,k2,k3)=bigI(lambda,mu,l,m,k1,k2,k3)
           enddo
           enddo
           enddo
        enddo
       enddo

       do lambdap=0,l+ntotB
        do mup=-lambdap,lambdap
           do k1p=0,n_b(1)
           do k2p=0,n_b(2)
           do k3p=0,n_b(3)
                  array_I_B(lambdap,mup,k1p,k2p,k3p)=bigI(lambdap,mup,l,m,k1p,k2p,k3p)
            enddo
            enddo
            enddo
        enddo
       enddo

       do lambda=0,l+ntotA
        do mu=-lambda,lambda

          do k1=0,n_a(1)
          do k2=0,n_a(2)
          do k3=0,n_a(3)

              prod=ylm(lambda,mu,theta_AC0,phi_AC0)*array_coefs_A(k1,k2,k3)*array_I_A(lambda,mu,k1,k2,k3)

              do lambdap=0,l+ntotB
               do mup=-lambdap,lambdap

                    do k1p=0,n_b(1)
                    do k2p=0,n_b(2)
                    do k3p=0,n_b(3)

                        prodp=ylm(lambdap,mup,theta_BC0,phi_BC0)*array_coefs_B(k1p,k2p,k3p)*array_I_B(lambdap,mup,k1p,k2p,k3p)

                        do k=1,kmax
                            ktot=k1+k2+k3+k1p+k2p+k3p+n_kl(k,l)
                            accu=accu+prod*prodp*v_kl(k,l)*array_R(ktot,k,l,lambda,lambdap)
                        enddo

                    enddo
                    enddo
                    enddo

               enddo
              enddo

          enddo
          enddo
          enddo

        enddo
       enddo

  enddo
 enddo

 !=!=!=!=!
 ! E n d !
 !=!=!=!=!

 Vpseudo=f*accu

else if(ac.eq.0.d0.and.bc.ne.0.d0)then

 !=!=!=!=!=!
 ! I n i t !
 !=!=!=!=!=!

 f=fourpi**1.5d0
 theta_BC0=dacos( (b(3)-c(3))/bc )
 phi_BC0=datan2((b(2)-c(2))/bc,(b(1)-c(1))/bc)

 areal=2.d0*g_a*ac
 breal=2.d0*g_b*bc
 freal=dexp(-g_a*ac**2-g_b*bc**2)

 do ktot=0,ntotA+ntotB+nkl_max
  do lambdap=0,lmax+ntotB
    do k=1,kmax
      do l=0,lmax

        array_R(ktot,k,l,0,lambdap)= freal & 
                                     *int_prod_bessel(ktot+2,g_a+g_b+dz_kl(k,l),0,lambdap,areal,breal)
      enddo
    enddo
  enddo
 enddo

 do k1p=0,n_b(1)
 do k2p=0,n_b(2)
 do k3p=0,n_b(3)

  array_coefs_B(k1p,k2p,k3p)=binom(n_b(1),k1p)*binom(n_b(2),k2p)*binom(n_b(3),k3p)  &
                             *(c(1)-b(1))**(n_b(1)-k1p)*(c(2)-b(2))**(n_b(2)-k2p)*(c(3)-b(3))**(n_b(3)-k3p)
 enddo
 enddo
 enddo

 !=!=!=!=!=!=!=!
 ! c a l c u l !
 !=!=!=!=!=!=!=!

 accu=0.d0
 do l=0,lmax
  do m=-l,l

    do lambdap=0,l+ntotB
      do mup=-lambdap,lambdap
        do k1p=0,n_b(1)
        do k2p=0,n_b(2)
        do k3p=0,n_b(3)
          array_I_B(lambdap,mup,k1p,k2p,k3p)=bigI(lambdap,mup,l,m,k1p,k2p,k3p)
        enddo
        enddo
        enddo
      enddo
     enddo
    
    prod=bigI(0,0,l,m,n_a(1),n_a(2),n_a(3))
    
    do lambdap=0,l+ntotB
      do mup=-lambdap,lambdap
         do k1p=0,n_b(1)
         do k2p=0,n_b(2)
         do k3p=0,n_b(3)
    
          prodp=array_coefs_B(k1p,k2p,k3p)*ylm(lambdap,mup,theta_BC0,phi_BC0)*array_I_B(lambdap,mup,k1p,k2p,k3p)
    
          do k=1,kmax

            ktot=ntotA+k1p+k2p+k3p+n_kl(k,l)
            accu=accu+prod*prodp*v_kl(k,l)*array_R(ktot,k,l,0,lambdap)

          enddo

         enddo
         enddo
         enddo
     enddo
    enddo
  enddo
 enddo

 !=!=!=!=!
 ! E n d !
 !=!=!=!=!

 Vpseudo=f*accu

else if(ac.ne.0.d0.and.bc.eq.0.d0)then

 !=!=!=!=!=!
 ! I n i t !
 !=!=!=!=!=!

 f=fourpi**1.5d0
 theta_AC0=dacos( (a(3)-c(3))/ac )
 phi_AC0=datan2((a(2)-c(2))/ac,(a(1)-c(1))/ac)

 areal=2.d0*g_a*ac
 breal=2.d0*g_b*bc
 freal=dexp(-g_a*ac**2-g_b*bc**2)

 do ktot=0,ntotA+ntotB+nkl_max
  do lambda=0,lmax+ntotA
    do k=1,kmax
      do l=0,lmax

      array_R(ktot,k,l,lambda,0)= freal & 
                                  *int_prod_bessel(ktot+2,g_a+g_b+dz_kl(k,l),lambda,0,areal,breal)

      enddo
    enddo
  enddo
 enddo

 do k1=0,n_a(1)
 do k2=0,n_a(2)
 do k3=0,n_a(3)

  array_coefs_A(k1,k2,k3)=binom(n_a(1),k1)*binom(n_a(2),k2)*binom(n_a(3),k3)  &
                          *(c(1)-a(1))**(n_a(1)-k1)*(c(2)-a(2))**(n_a(2)-k2)*(c(3)-a(3))**(n_a(3)-k3)

 enddo
 enddo
 enddo

 !=!=!=!=!=!=!=!
 ! c a l c u l !
 !=!=!=!=!=!=!=!

 accu=0.d0
 do l=0,lmax
  do m=-l,l

    do lambda=0,l+ntotA
      do mu=-lambda,lambda
        do k1=0,n_a(1)
        do k2=0,n_a(2)
        do k3=0,n_a(3)
          array_I_A(lambda,mu,k1,k2,k3)=bigI(lambda,mu,l,m,k1,k2,k3)
        enddo
        enddo
        enddo
      enddo
    enddo

    do lambda=0,l+ntotA
      do mu=-lambda,lambda
        do k1=0,n_a(1)
        do k2=0,n_a(2)
        do k3=0,n_a(3)

          prod=array_coefs_A(k1,k2,k3)*ylm(lambda,mu,theta_AC0,phi_AC0)*array_I_A(lambda,mu,k1,k2,k3)
          prodp=bigI(0,0,l,m,n_b(1),n_b(2),n_b(3))

          do k=1,kmax
            ktot=k1+k2+k3+ntotB+n_kl(k,l)
            accu=accu+prod*prodp*v_kl(k,l)*array_R(ktot,k,l,lambda,0)
          enddo

        enddo
        enddo
        enddo
     enddo
    enddo

  enddo
 enddo
 
 !=!=!=!=!
 ! E n d !
 !=!=!=!=!

 Vpseudo=f*accu
endif

!  _                      
! |_ o ._   _. | o  _  _  
! |  | | | (_| | | _> (/_ 
!                         
  deallocate (array_R, array_I_A, array_I_B)
  return
end

!                                  _                               
!                                 | |                              
!__   __  _ __  ___  ___ _   _  __| | ___    _ __  _   _ _ __ ___  
!\ \ / / | '_ \/ __|/ _ \ | | |/ _` |/ _ \  | '_ \| | | | '_ ` _ \ 
! \ V /  | |_) \__ \  __/ |_| | (_| | (_) | | | | | |_| | | | | | |
!  \_/   | .__/|___/\___|\__,_|\__,_|\___/  |_| |_|\__,_|_| |_| |_|
!        | |                                                       
!        |_|                                                       

double precision function Vpseudo_num(npts,rmax,lmax,kmax,v_kl,n_kl,dz_kl,a,n_a,g_a,b,n_b,g_b,c)
implicit none
integer kmax_max,lmax_max
parameter (kmax_max=2,lmax_max=2)
integer lmax,kmax, n_kl(kmax_max,0:lmax_max),l,m,k,kk
double precision v_kl(kmax_max,0:lmax_max),dz_kl(kmax_max,0:lmax_max)
double precision a(3),g_a,b(3),g_b,c(3),ac(3),bc(3)
integer n_a(3),n_b(3),npts
double precision rmax,dr,sum,rC
double precision overlap_orb_ylm_brute

do l=1,3
 ac(l)=a(l)-c(l)
 bc(l)=b(l)-c(l)
enddo

dr=rmax/npts
sum=0.d0
do l=0,lmax
 do m=-l,l
  do k=1,npts
   rC=(k-1)*dr+dr/2.d0
   do kk=1,kmax
    sum=sum+dr*v_kl(kk,l)*rC**(n_kl(kk,l)+2)*dexp(-dz_kl(kk,l)*rC**2)   &
    *overlap_orb_ylm_brute(npts,rC,n_a,ac,g_a,l,m)   &
    *overlap_orb_ylm_brute(npts,rC,n_b,bc,g_b,l,m)
   enddo
  enddo
 enddo
enddo
Vpseudo_num=sum
return
end
!! Routine Vloc is a variation of formumla (66) 
!! of Kahn Baybutt TRuhlar J.Chem.Phys. vol.65 3826 (1976)
!! without the projection operator
!!
!! Vloc= (4pi)**3/2* \sum_{k=1}^{klocmax}  \sum_{l=0}^lmax \sum_{m=-l}^{l}
!!\sum{k1=0}^{nAx} \sum{k2=0}^{nAy} \sum{k3=0}^{nAz}
!! binom(nAx,k1)*binom(nAy,k2)*binom(nAz,k3)
!! *CAx**(nAx-k1)*CAy**(nAy-k2)*CAz**(nAz-k3)*
!! \sum{k1p=0}^{nBx} \sum{k2p=0}^{nBy} \sum{k3p=0}^{nBz}
!! binom(nBx,k1p)*binom(nBy,k2p)*binom(nBz,k3p)
!! *CBx**(nBx-k1p)*CBy**(nBy-k2p)*CBz**(nBz-k3p)*
!!\sum_{l=0}^lmax \sum_{m=-l}^{l}

!! bigI(0,0,l,m,k1+k1p,k2+k2p,k3+k3p)*Y_{l m}(D_unit)
!! *v_k(k)* bigR(lambda,k1+k2+k3+k1p+k2p+k3p+n_k(k),g_a,g_b,AC,BC,dz_k(k))
!!
!! nA=nAx+nAy+nAz
!! nB=nBx+nBy+nBz
!! D=(g_a AC+g_b BC)
!! D_unit= vect(D)/D
!! AC_x= A_x-C_x, etc.
!! BC=|B-C|
!! AC_unit= vect(AC)/AC
!! BC_unit= vect(BC)/BCA
!!
!! bigR(lambda,g_a,g_b,g_k,AC,BC)
!! = exp(-g_a* AC**2 -g_b* BC**2)* 
!!    I_loc= \int dx x**l *exp(-gam*x**2) M_n(ax)  l=ktot+2 gam=g_a+g_b+dz_k(k) a=dreal n=l 
!!    M_n(x) modified spherical bessel function 


double precision function Vloc(klocmax,v_k,n_k,dz_k,a,n_a,g_a,b,n_b,g_b,c)
implicit none
integer klocmax_max,lmax_max,ntot_max
parameter (klocmax_max=10,lmax_max=2)
parameter (ntot_max=10)
integer klocmax
double precision v_k(klocmax_max),dz_k(klocmax_max),crochet,bigA
integer n_k(klocmax_max)
double precision a(3),g_a,b(3),g_b,c(3),d(3)
integer n_a(3),n_b(3),ntotA,ntotB,ntot,m
integer i,l,k,ktot,k1,k2,k3,k1p,k2p,k3p
double precision f,fourpi,ac,bc,freal,d2,dreal,theta_DC0,phi_DC0
double precision,allocatable :: array_R_loc(:,:,:)
double precision,allocatable :: array_coefs(:,:,:,:,:,:)
double precision int_prod_bessel_loc,binom,accu,prod,ylm,bigI,arg

 fourpi=4.d0*dacos(-1.d0)
 f=fourpi**1.5d0
 ac=dsqrt((a(1)-c(1))**2+(a(2)-c(2))**2+(a(3)-c(3))**2)
 bc=dsqrt((b(1)-c(1))**2+(b(2)-c(2))**2+(b(3)-c(3))**2)
 arg=g_a*ac**2+g_b*bc**2
 if(arg.gt.-dlog(10.d-20))then
 Vloc=0.d0
 return
 endif

 ntotA=n_a(1)+n_a(2)+n_a(3)
 ntotB=n_b(1)+n_b(2)+n_b(3)
 ntot=ntotA+ntotB

 if(ac.eq.0.d0.and.bc.eq.0.d0)then
  accu=0.d0

  do k=1,klocmax 
   accu=accu+v_k(k)*crochet(n_k(k)+2+ntot,g_a+g_b+dz_k(k))
  enddo
  Vloc=accu*fourpi*bigI(0,0,0,0,n_a(1)+n_b(1),n_a(2)+n_b(2),n_a(3)+n_b(3))
  !bigI frequantly is null
  return
 endif

 freal=dexp(-g_a*ac**2-g_b*bc**2)
 
 d2=0.d0
 do i=1,3
  d(i)=g_a*(a(i)-c(i))+g_b*(b(i)-c(i))
  d2=d2+d(i)**2
 enddo
 d2=dsqrt(d2)
 dreal=2.d0*d2

 theta_DC0=dacos(d(3)/d2)
 phi_DC0=datan2(d(2)/d2,d(1)/d2)

allocate (array_R_loc(-2:ntot_max+klocmax_max,klocmax_max,0:ntot_max))
allocate (array_coefs(0:ntot_max,0:ntot_max,0:ntot_max,0:ntot_max,0:ntot_max,0:ntot_max))

 do ktot=-2,ntotA+ntotB+klocmax
 do l=0,ntot
 do k=1,klocmax
  array_R_loc(ktot,k,l)=freal*int_prod_bessel_loc(ktot+2,g_a+g_b+dz_k(k),l,dreal)
 enddo
 enddo
 enddo

 do k1=0,n_a(1)
 do k2=0,n_a(2)
 do k3=0,n_a(3)
 do k1p=0,n_b(1)
 do k2p=0,n_b(2)
 do k3p=0,n_b(3)
  array_coefs(k1,k2,k3,k1p,k2p,k3p)=binom(n_a(1),k1)*binom(n_a(2),k2)*binom(n_a(3),k3)  &
  *(c(1)-a(1))**(n_a(1)-k1)*(c(2)-a(2))**(n_a(2)-k2)*(c(3)-a(3))**(n_a(3)-k3) &
  *binom(n_b(1),k1p)*binom(n_b(2),k2p)*binom(n_b(3),k3p)  &
  *(c(1)-b(1))**(n_b(1)-k1p)*(c(2)-b(2))**(n_b(2)-k2p)*(c(3)-b(3))**(n_b(3)-k3p)
 enddo
 enddo
 enddo
 enddo
 enddo
 enddo

 accu=0.d0
 do k=1,klocmax
  do k1=0,n_a(1)
  do k2=0,n_a(2)
  do k3=0,n_a(3)
  do k1p=0,n_b(1)
  do k2p=0,n_b(2)
  do k3p=0,n_b(3)

  do l=0,ntot
   do m=-l,l
    prod=ylm(l,m,theta_DC0,phi_DC0)*array_coefs(k1,k2,k3,k1p,k2p,k3p) &
         *bigI(l,m,0,0,k1+k1p,k2+k2p,k3+k3p)
    ktot=k1+k2+k3+k1p+k2p+k3p+n_k(k)
    accu=accu+prod*v_k(k)*array_R_loc(ktot,k,l)
   enddo
  enddo

  enddo
  enddo
  enddo
  enddo
  enddo
  enddo
 enddo
 Vloc=f*accu

 deallocate (array_R_loc)
 deallocate (array_coefs)
end

double precision function bigA(i,j,k)
implicit none
integer i,j,k
double precision fourpi,dblefact
fourpi=4.d0*dacos(-1.d0)
bigA=0.d0
if(mod(i,2).eq.1)return
if(mod(j,2).eq.1)return
if(mod(k,2).eq.1)return
bigA=fourpi*dblefact(i-1)*dblefact(j-1)*dblefact(k-1)/dblefact(i+j+k+1)
end
!!
!! I_{lambda,mu,l,m}^{k1,k2,k3} = /int dOmega  Y_{lambda mu} xchap^k1 ychap^k2 zchap^k3  Y_{lm}
!!

double precision function bigI(lambda,mu,l,m,k1,k2,k3)
implicit none
integer lambda,mu,l,m,k1,k2,k3
integer k,i,kp,ip
double precision pi,sum,factor1,factor2,cylm,cylmp,bigA,binom,fact,coef_pm
pi=dacos(-1.d0)

if(mu.gt.0.and.m.gt.0)then
sum=0.d0
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
do k=0,mu/2
 do i=0,lambda-mu
  do kp=0,m/2
   do ip=0,l-m
    cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
    cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
    sum=sum+cylm*cylmp*bigA(mu-2*k+m-2*kp+k1,2*k+2*kp+k2,i+ip+k3)
   enddo
  enddo
 enddo
enddo
bigI=sum
return
endif

if(mu.eq.0.and.m.eq.0)then
factor1=dsqrt((2*lambda+1)/(4.d0*pi))
factor2=dsqrt((2*l+1)/(4.d0*pi))
sum=0.d0
do i=0,lambda
 do ip=0,l
  cylm=factor1*coef_pm(lambda,i)
  cylmp=factor2*coef_pm(l,ip)
  sum=sum+cylm*cylmp*bigA(k1,k2,i+ip+k3)
 enddo
enddo
bigI=sum
return
endif

if(mu.eq.0.and.m.gt.0)then
factor1=dsqrt((2*lambda+1)/(4.d0*pi))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
sum=0.d0
do i=0,lambda
 do kp=0,m/2
  do ip=0,l-m
   cylm=factor1*coef_pm(lambda,i)
   cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
   sum=sum+cylm*cylmp*bigA(m-2*kp+k1,2*kp+k2,i+ip+k3)
  enddo
 enddo
enddo
bigI=sum
return
endif

if(mu.gt.0.and.m.eq.0)then
sum=0.d0
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)/(4.d0*pi))
do k=0,mu/2
 do i=0,lambda-mu
  do ip=0,l
   cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
   cylmp=factor2*coef_pm(l,ip)
   sum=sum+cylm*cylmp*bigA(mu-2*k +k1,2*k +k2,i+ip +k3)
  enddo
 enddo
enddo
bigI=sum
return
endif

if(mu.lt.0.and.m.lt.0)then
mu=-mu
m=-m
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
sum=0.d0
do k=0,(mu-1)/2
 do i=0,lambda-mu
  do kp=0,(m-1)/2
   do ip=0,l-m
    cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k+1)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
    cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp+1)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
    sum=sum+cylm*cylmp*bigA(mu-(2*k+1)+m-(2*kp+1)+k1,(2*k+1)+(2*kp+1)+k2,i+ip+k3)
   enddo
  enddo
 enddo
enddo
mu=-mu
m=-m
bigI=sum
return
endif

if(mu.eq.0.and.m.lt.0)then
m=-m
factor1=dsqrt((2*lambda+1)/(4.d0*pi))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
sum=0.d0
do i=0,lambda
 do kp=0,(m-1)/2
  do ip=0,l-m
   cylm=factor1*coef_pm(lambda,i)
   cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp+1)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
   sum=sum+cylm*cylmp*bigA(m-(2*kp+1)+k1,2*kp+1+k2,i+ip+k3)
  enddo
 enddo
enddo
m=-m
bigI=sum
return
endif

if(mu.lt.0.and.m.eq.0)then
sum=0.d0
mu=-mu
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)/(4.d0*pi))
do k=0,(mu-1)/2
 do i=0,lambda-mu
   do ip=0,l
    cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k+1)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
    cylmp=factor2*coef_pm(l,ip)
    sum=sum+cylm*cylmp*bigA(mu-(2*k+1)+k1,2*k+1+k2,i+ip+k3)
   enddo
 enddo
enddo
mu=-mu
bigI=sum
return
endif

if(mu.gt.0.and.m.lt.0)then
sum=0.d0
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
m=-m
do k=0,mu/2
 do i=0,lambda-mu
  do kp=0,(m-1)/2
   do ip=0,l-m
    cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
    cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp+1)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
    sum=sum+cylm*cylmp*bigA(mu-2*k+m-(2*kp+1)+k1,2*k+2*kp+1+k2,i+ip+k3)
   enddo
  enddo
 enddo
enddo
m=-m
bigI=sum
return
endif

if(mu.lt.0.and.m.gt.0)then
mu=-mu
factor1=dsqrt((2*lambda+1)*fact(lambda-iabs(mu))/(4.d0*pi*fact(lambda+iabs(mu))))
factor2=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
sum=0.d0
do k=0,(mu-1)/2
 do i=0,lambda-mu
  do kp=0,m/2
   do ip=0,l-m
    cylm=(-1.d0)**k*factor1*dsqrt(2.d0)*binom(mu,2*k+1)*fact(mu+i)/fact(i)*coef_pm(lambda,i+mu)
    cylmp=(-1.d0)**kp*factor2*dsqrt(2.d0)*binom(m,2*kp)*fact(m+ip)/fact(ip)*coef_pm(l,ip+m)
    sum=sum+cylm*cylmp*bigA(mu-(2*k+1)+m-2*kp+k1,2*k+1+2*kp+k2,i+ip+k3)
   enddo
  enddo
 enddo
enddo
bigI=sum
mu=-mu
return
endif

stop 'pb in bigI!'
end

double precision function crochet(n,g)
implicit none
integer n
double precision g,pi,dblefact,expo
pi=dacos(-1.d0)
expo=0.5d0*dfloat(n+1)
crochet=dblefact(n-1)/(2.d0*g)**expo
if(mod(n,2).eq.0)crochet=crochet*dsqrt(pi/2.d0)
end

!!
!! overlap= <phi|Ylm>= /int dOmega Ylm (x-center_x)**nx*(y-center_y)**nx*(z-center)**nx
!!                                     *exp(-g*(r-center)**2)
!!
double precision function overlap_orb_ylm_brute(npts,r,npower_orb,center_orb,g_orb,l,m)
implicit none
integer npower_orb(3),l,m,i,j,npts
double precision u,g_orb,du,dphi,term,orb_phi,ylm_real,sintheta,r_orb,phi,center_orb(3)
double precision x_orb,y_orb,z_orb,twopi,r
twopi=2.d0*dacos(-1.d0)
du=2.d0/npts
dphi=twopi/npts
overlap_orb_ylm_brute=0.d0
do i=1,npts
 u=-1.d0+du*(i-1)+du/2.d0
 sintheta=dsqrt(1.d0-u**2)
 do j=1,npts
  phi=dphi*(j-1)+dphi/2.d0
  x_orb=r*dcos(phi)*sintheta 
  y_orb=r*dsin(phi)*sintheta
  z_orb=r*u
  term=orb_phi(x_orb,y_orb,z_orb,npower_orb,center_orb,g_orb)*ylm_real(l,m,u,phi)
  overlap_orb_ylm_brute= overlap_orb_ylm_brute+term*du*dphi
 enddo
enddo
end

double precision function overlap_orb_ylm_grid(nptsgrid,r_orb,npower_orb,center_orb,g_orb,l,m)
implicit none
!! PSEUDOS
integer nptsgridmax,nptsgrid
double precision coefs_pseudo,ptsgrid
parameter(nptsgridmax=50)
common/pseudos/coefs_pseudo(nptsgridmax),ptsgrid(nptsgridmax,3)
!!!!!
integer npower_orb(3),l,m,i
double precision x,g_orb,two_pi,dx,dphi,term,orb_phi,ylm_real,sintheta,r_orb,phi,center_orb(3)
double precision x_orb,y_orb,z_orb,twopi,pi,cosphi,sinphi,xbid
pi=dacos(-1.d0)
twopi=2.d0*pi
overlap_orb_ylm_grid=0.d0
do i=1,nptsgrid
 x_orb=r_orb*ptsgrid(i,1)
 y_orb=r_orb*ptsgrid(i,2)
 z_orb=r_orb*ptsgrid(i,3)
 x=ptsgrid(i,3)
 phi=datan2(ptsgrid(i,2),ptsgrid(i,1))
 term=orb_phi(x_orb,y_orb,z_orb,npower_orb,center_orb,g_orb)*ylm_real(l,m,x,phi)
 overlap_orb_ylm_grid= overlap_orb_ylm_grid+coefs_pseudo(i)*term
enddo
overlap_orb_ylm_grid=2.d0*twopi*overlap_orb_ylm_grid
end

!  Y_l^m(theta,phi) = i^(m+|m|) ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2  P_l^|m|(cos(theta))  exp(i m phi)
!  l=0,1,2,....
!  m=0,1,...,l
! Here:
!  n=l (n=0,1,...)  
!  m=0,1,...,n 
!  x=cos(theta) 0 < x < 1
!
!  
!  This routine computes:   PM(m,n) for n=0,...,N (number N in input) and m=0,..,n

!   Exemples (see 'Associated Legendre Polynomilas wikipedia')
!    P_{0}^{0}(x)=1
!    P_{1}^{-1}(x)=-1/2 P_{1}^{1}(x)
!    P_{1}^{0}(x)=x
!    P_{1}^{1}(x)=-(1-x^2)^{1/2}
!    P_{2}^{-2}(x)=1/24 P_{2}^{2}(x)
!    P_{2}^{-1}(x)=-1/6 P_{2}^{1}(x)
!    P_{2}^{0}(x)=1/2 (3x^{2}-1)
!    P_{2}^{1}(x)=-3x(1-x^2)^{1/2}
!    P_{2}^{2}(x)=3(1-x^2)


        SUBROUTINE LPMN(MM,M,N,X,PM)
!
! Here N = LMAX
! Here M= MMAX (we take M=LMAX in input)
!
!       =====================================================
!       Purpose: Compute the associated Legendre functions Pmn(x)
!       Input :  x  --- Argument of Pmn(x)
!                m  --- Order of Pmn(x),  m = 0,1,2,...,n
!                n  --- Degree of Pmn(x), n = 0,1,2,...,N
!                mm --- Physical dimension of PM 
!       Output:  PM(m,n) --- Pmn(x)
!       =====================================================
!
        IMPLICIT DOUBLE PRECISION (P,X)
        DIMENSION PM(0:MM,0:(N+1))
        DO 10 I=0,N
        DO 10 J=0,M
10         PM(J,I)=0.0D0
        PM(0,0)=1.0D0
        IF (DABS(X).EQ.1.0D0) THEN
           DO 15 I=1,N
15            PM(0,I)=X**I
           RETURN
        ENDIF
        LS=1
        IF (DABS(X).GT.1.0D0) LS=-1
        XQ=DSQRT(LS*(1.0D0-X*X))
        XS=LS*(1.0D0-X*X)
        DO 30 I=1,M
30         PM(I,I)=-LS*(2.0D0*I-1.0D0)*XQ*PM(I-1,I-1)
        DO 35 I=0,M
35         PM(I,I+1)=(2.0D0*I+1.0D0)*X*PM(I,I)

        DO 40 I=0,M
        DO 40 J=I+2,N
           PM(I,J)=((2.0D0*J-1.0D0)*X*PM(I,J-1)- (I+J-1.0D0)*PM(I,J-2))/(J-I)
40      CONTINUE
        END


!  Y_l^m(theta,phi) = i^(m+|m|) ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2
!  P_l^|m|(cos(theta))  exp(i m phi)

      subroutine erreur(x,n,rmoy,error)
      implicit double precision(a-h,o-z)
      dimension x(n)
! calcul de la moyenne
      rmoy=0.d0
      do i=1,n
        rmoy=rmoy+x(i)
      enddo
      rmoy=rmoy/dfloat(n)
! calcul de l'erreur
      error=0.d0
      do i=1,n
       error=error+(x(i)-rmoy)**2
      enddo
      if(n.gt.1)then
        rn=dfloat(n)
        rn1=dfloat(n-1)
        error=dsqrt(error)/dsqrt(rn*rn1)
      else
        write(2,*)'Seulement un block Erreur nondefinie'
        error=0.d0
      endif
      end

      subroutine initpseudos(nptsgrid)
      implicit none
      integer nptsgridmax,nptsgrid,ik
      double precision coefs_pseudo,ptsgrid
      double precision p,q,r,s
      parameter(nptsgridmax=50)
      common/pseudos/coefs_pseudo(nptsgridmax),ptsgrid(nptsgridmax,3)

      p=1.d0/dsqrt(2.d0)
      q=1.d0/dsqrt(3.d0)
      r=1.d0/dsqrt(11.d0)
      s=3.d0/dsqrt(11.d0)

      if(nptsgrid.eq.4)then

        ptsgrid(1,1)=q
        ptsgrid(1,2)=q
        ptsgrid(1,3)=q

        ptsgrid(2,1)=q
        ptsgrid(2,2)=-q
        ptsgrid(2,3)=-q

        ptsgrid(3,1)=-q
        ptsgrid(3,2)=q
        ptsgrid(3,3)=-q

        ptsgrid(4,1)=-q
        ptsgrid(4,2)=-q
        ptsgrid(4,3)=q

        do ik=1,4
         coefs_pseudo(ik)=1.d0/4.d0
        enddo
        return
       endif

       ptsgrid(1,1)=1.d0
       ptsgrid(1,2)=0.d0
       ptsgrid(1,3)=0.d0

       ptsgrid(2,1)=-1.d0
       ptsgrid(2,2)=0.d0
       ptsgrid(2,3)=0.d0

       ptsgrid(3,1)=0.d0
       ptsgrid(3,2)=1.d0
       ptsgrid(3,3)=0.d0

       ptsgrid(4,1)=0.d0
       ptsgrid(4,2)=-1.d0
       ptsgrid(4,3)=0.d0

       ptsgrid(5,1)=0.d0
       ptsgrid(5,2)=0.d0
       ptsgrid(5,3)=1.d0

       ptsgrid(6,1)=0.d0
       ptsgrid(6,2)=0.d0
       ptsgrid(6,3)=-1.d0

       do ik=1,6
        coefs_pseudo(ik)=1.d0/6.d0
       enddo

       if(nptsgrid.eq.6)return

       ptsgrid(7,1)=p
       ptsgrid(7,2)=p
       ptsgrid(7,3)=0.d0

       ptsgrid(8,1)=p
       ptsgrid(8,2)=-p
       ptsgrid(8,3)=0.d0

       ptsgrid(9,1)=-p
       ptsgrid(9,2)=p
       ptsgrid(9,3)=0.d0

       ptsgrid(10,1)=-p
       ptsgrid(10,2)=-p
       ptsgrid(10,3)=0.d0

       ptsgrid(11,1)=p
       ptsgrid(11,2)=0.d0
       ptsgrid(11,3)=p

       ptsgrid(12,1)=p
       ptsgrid(12,2)=0.d0
       ptsgrid(12,3)=-p

       ptsgrid(13,1)=-p
       ptsgrid(13,2)=0.d0
       ptsgrid(13,3)=p

       ptsgrid(14,1)=-p
       ptsgrid(14,2)=0.d0
       ptsgrid(14,3)=-p

       ptsgrid(15,1)=0.d0
       ptsgrid(15,2)=p
       ptsgrid(15,3)=p

       ptsgrid(16,1)=0.d0
       ptsgrid(16,2)=p
       ptsgrid(16,3)=-p

       ptsgrid(17,1)=0.d0
       ptsgrid(17,2)=-p
       ptsgrid(17,3)=p

       ptsgrid(18,1)=0.d0
       ptsgrid(18,2)=-p
       ptsgrid(18,3)=-p

       do ik=1,6
        coefs_pseudo(ik)=1.d0/30.d0
       enddo
       do ik=7,18
        coefs_pseudo(ik)=1.d0/15.d0
       enddo

       if(nptsgrid.eq.18)return

       ptsgrid(19,1)=q
       ptsgrid(19,2)=q
       ptsgrid(19,3)=q

       ptsgrid(20,1)=-q
       ptsgrid(20,2)=q
       ptsgrid(20,3)=q

       ptsgrid(21,1)=q
       ptsgrid(21,2)=-q
       ptsgrid(21,3)=q

       ptsgrid(22,1)=q
       ptsgrid(22,2)=q
       ptsgrid(22,3)=-q

       ptsgrid(23,1)=-q
       ptsgrid(23,2)=-q
       ptsgrid(23,3)=q

       ptsgrid(24,1)=-q
       ptsgrid(24,2)=q
       ptsgrid(24,3)=-q

       ptsgrid(25,1)=q
       ptsgrid(25,2)=-q
       ptsgrid(25,3)=-q

       ptsgrid(26,1)=-q
       ptsgrid(26,2)=-q
       ptsgrid(26,3)=-q

       do ik=1,6
        coefs_pseudo(ik)=1.d0/21.d0
       enddo
       do ik=7,18
        coefs_pseudo(ik)=4.d0/105.d0
       enddo
       do ik=19,26
        coefs_pseudo(ik)=27.d0/840.d0
       enddo

       if(nptsgrid.eq.26)return

       ptsgrid(27,1)=r
       ptsgrid(27,2)=r
       ptsgrid(27,3)=s

       ptsgrid(28,1)=r
       ptsgrid(28,2)=-r
       ptsgrid(28,3)=s

       ptsgrid(29,1)=-r
       ptsgrid(29,2)=r
       ptsgrid(29,3)=s

       ptsgrid(30,1)=-r
       ptsgrid(30,2)=-r
       ptsgrid(30,3)=s

       ptsgrid(31,1)=r
       ptsgrid(31,2)=r
       ptsgrid(31,3)=-s

       ptsgrid(32,1)=r
       ptsgrid(32,2)=-r
       ptsgrid(32,3)=-s

       ptsgrid(33,1)=-r
       ptsgrid(33,2)=r
       ptsgrid(33,3)=-s

       ptsgrid(34,1)=-r
       ptsgrid(34,2)=-r
       ptsgrid(34,3)=-s

       ptsgrid(35,1)=r
       ptsgrid(35,2)=s
       ptsgrid(35,3)=r

       ptsgrid(36,1)=-r
       ptsgrid(36,2)=s
       ptsgrid(36,3)=r

       ptsgrid(37,1)=r
       ptsgrid(37,2)=s
       ptsgrid(37,3)=-r

       ptsgrid(38,1)=-r
       ptsgrid(38,2)=s
       ptsgrid(38,3)=-r

       ptsgrid(39,1)=r
       ptsgrid(39,2)=-s
       ptsgrid(39,3)=r

       ptsgrid(40,1)=r
       ptsgrid(40,2)=-s
       ptsgrid(40,3)=-r

       ptsgrid(41,1)=-r
       ptsgrid(41,2)=-s
       ptsgrid(41,3)=r

       ptsgrid(42,1)=-r
       ptsgrid(42,2)=-s
       ptsgrid(42,3)=-r

       ptsgrid(43,1)=s
       ptsgrid(43,2)=r
       ptsgrid(43,3)=r

       ptsgrid(44,1)=s
       ptsgrid(44,2)=r
       ptsgrid(44,3)=-r

       ptsgrid(45,1)=s
       ptsgrid(45,2)=-r
       ptsgrid(45,3)=r

       ptsgrid(46,1)=s
       ptsgrid(46,2)=-r
       ptsgrid(46,3)=-r

       ptsgrid(47,1)=-s
       ptsgrid(47,2)=r
       ptsgrid(47,3)=r

       ptsgrid(48,1)=-s
       ptsgrid(48,2)=r
       ptsgrid(48,3)=-r

       ptsgrid(49,1)=-s
       ptsgrid(49,2)=-r
       ptsgrid(49,3)=r

       ptsgrid(50,1)=-s
       ptsgrid(50,2)=-r
       ptsgrid(50,3)=-r

       do ik=1,6
        coefs_pseudo(ik)=4.d0/315.d0
       enddo
       do ik=7,18
        coefs_pseudo(ik)=64.d0/2835.d0
       enddo
       do ik=19,26
        coefs_pseudo(ik)=27.d0/1280.d0
       enddo
       do ik=27,50
        coefs_pseudo(ik)=14641.d0/725760.d0
       enddo

       if(nptsgrid.eq.50)return

       write(*,*)'Grid for pseudos not available!'
       write(*,*)'N=4-6-18-26-50 only!'
       stop
      end

double precision function dblefact(n)
implicit none
integer :: n,k
double precision prod
dblefact=1.d0

if(n.lt.0)return
if(mod(n,2).eq.1)then
  prod=1.d0
  do k=1,n,2
   prod=prod*dfloat(k)
  enddo
  dblefact=prod
  return
 endif
 if(mod(n,2).eq.0)then
  prod=1.d0
  do k=2,n,2
   prod=prod*dfloat(k)
  enddo
  dblefact=prod
  return
 endif
end
!!
!! R_{lambda,lamba',N}= exp(-ga_a AC**2 -g_b BC**2) /int_{0}{+infty} r**(2+n) exp(-(g_a+g_b+g_k)r**2)
!!                       * M_{lambda}( 2g_a ac r) M_{lambda'}(2g_b bc r)
!!
        double precision function bigR(lambda,lambdap,n,g_a,g_b,ac,bc,g_k)
        implicit none
        integer lambda,lambdap,n,npts,i
        double precision g_a,g_b,ac,bc,g_k,arg,factor,delta1,delta2,cc,rmax,dr,sum,x1,x2,r
        double precision bessel_mod
        arg=g_a*ac**2+g_b*bc**2
        factor=dexp(-arg)
        delta1=2.d0*g_a*ac
        delta2=2.d0*g_b*bc
        cc=g_a+g_b+g_k
        if(cc.eq.0.d0)stop 'pb. in bigR'
        rmax=dsqrt(-dlog(10.d-20)/cc)
        npts=500 
        dr=rmax/npts
        sum=0.d0
        do i=1,npts
         r=(i-1)*dr
         x1=delta1*r
         x2=delta2*r
         sum=sum+dr*r**(n+2)*dexp(-cc*r**2)*bessel_mod(x1,lambda)*bessel_mod(x2,lambdap)
        enddo
        bigR=sum*factor
        end

       double precision function bessel_mod(x,n)
       implicit none
       integer n
       double precision x,bessel_mod_exp,bessel_mod_recur
       if(x.le.0.8d0)then
        bessel_mod=bessel_mod_exp(n,x)
       else
        bessel_mod=bessel_mod_recur(n,x)
       endif
       end

       recursive function bessel_mod_recur(n,x) result(a)
       implicit none
       integer n
       double precision x,a,bessel_mod_exp
       if(x.le.0.8d0)then
        a=bessel_mod_exp(n,x)
        return
       endif
       if(n.eq.0)a=dsinh(x)/x
       if(n.eq.1)a=(x*dcosh(x)-dsinh(x))/x**2
       if(n.ge.2)a=bessel_mod_recur(n-2,x)-(2*n-1)/x*bessel_mod_recur(n-1,x)
       end

       double precision function bessel_mod_exp(n,x)
       implicit none
       integer n,k
       double precision x,coef,accu,fact,dblefact
       accu=0.d0
       do k=0,10
        coef=1.d0/fact(k)/dblefact(2*(n+k)+1)
        accu=accu+(x**2/2.d0)**k*coef
       enddo
       bessel_mod_exp=x**n*accu
       end

!        double precision function bessel_mod(x,n)
!        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!        parameter(NBESSMAX=100)
!        dimension SI(0:NBESSMAX),DI(0:NBESSMAX)
!        if(n.lt.0.or.n.gt.NBESSMAX)stop 'pb with argument of bessel_mod'
!        CALL SPHI(N,X,NBESSMAX,SI,DI)
!        bessel_mod=si(n)
!        end

        SUBROUTINE SPHI(N,X,NMAX,SI,DI)
!
!       ========================================================
!       Purpose: Compute modified spherical Bessel functions
!                of the first kind, in(x) and in'(x)
!       Input :  x --- Argument of in(x)
!                n --- Order of in(x) ( n = 0,1,2,... )
!       Output:  SI(n) --- in(x)
!                DI(n) --- in'(x)
!                NM --- Highest order computed
!       Routines called:
!                MSTA1 and MSTA2 for computing the starting
!                point for backward recurrence
!       ========================================================
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION SI(0:NMAX),DI(0:NMAX)
        NM=N
        IF (DABS(X).LT.1.0D-100) THEN
           DO 10 K=0,N
              SI(K)=0.0D0
10            DI(K)=0.0D0
           SI(0)=1.0D0
           DI(1)=0.333333333333333D0
           RETURN
        ENDIF
        SI(0)=DSINH(X)/X
        SI(1)=-(DSINH(X)/X-DCOSH(X))/X
        SI0=SI(0)
        IF (N.GE.2) THEN
           M=MSTA1(X,200)

           write(34,*)'m=',m

           IF (M.LT.N) THEN
              NM=M
           ELSE
              M=MSTA2(X,N,15)
           write(34,*)'m=',m
           ENDIF
           F0=0.0D0
           F1=1.0D0-100
           DO 15 K=M,0,-1
              F=(2.0D0*K+3.0D0)*F1/X+F0
              IF (K.LE.NM) SI(K)=F
              F0=F1
15            F1=F
           CS=SI0/F
           write(34,*)'cs=',cs
           DO 20 K=0,NM
20            SI(K)=CS*SI(K)
        ENDIF
        DI(0)=SI(1)
        DO 25 K=1,NM
25         DI(K)=SI(K-1)-(K+1.0D0)/X*SI(K)
        RETURN
        END


        INTEGER FUNCTION MSTA1(X,MP)
!
!       ===================================================
!       Purpose: Determine the starting point for backward  
!                recurrence such that the magnitude of    
!                Jn(x) at that point is about 10^(-MP)
!       Input :  x     --- Argument of Jn(x)
!                MP    --- Value of magnitude
!       Output:  MSTA1 --- Starting point   
!       ===================================================
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        A0=DABS(X)
        N0=INT(1.1*A0)+1
        F0=ENVJ(N0,A0)-MP
        N1=N0+5
        F1=ENVJ(N1,A0)-MP
        DO 10 IT=1,20             
           NN=N1-(N1-N0)/(1.0D0-F0/F1)                  
           F=ENVJ(NN,A0)-MP
           IF(ABS(NN-N1).LT.1) GO TO 20
           N0=N1
           F0=F1
           N1=NN
 10        F1=F
 20     MSTA1=NN
        RETURN
        END


        INTEGER FUNCTION MSTA2(X,N,MP)
!
!       ===================================================
!       Purpose: Determine the starting point for backward
!                recurrence such that all Jn(x) has MP
!                significant digits
!       Input :  x  --- Argument of Jn(x)
!                n  --- Order of Jn(x)
!                MP --- Significant digit
!       Output:  MSTA2 --- Starting point
!       ===================================================
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        A0=DABS(X)
        HMP=0.5D0*MP
        EJN=ENVJ(N,A0)
        IF (EJN.LE.HMP) THEN
           OBJ=MP
           N0=INT(1.1*A0)
        ELSE
           OBJ=HMP+EJN
           N0=N
        ENDIF
        F0=ENVJ(N0,A0)-OBJ
        N1=N0+5
        F1=ENVJ(N1,A0)-OBJ
        DO 10 IT=1,20
           NN=N1-(N1-N0)/(1.0D0-F0/F1)
           F=ENVJ(NN,A0)-OBJ
           IF (iABS(NN-N1).LT.1) GO TO 20
           N0=N1
           F0=F1
           N1=NN
10         F1=F
20      MSTA2=NN+10
        RETURN
        END

        double precision FUNCTION ENVJ(N,X)
        DOUBLE PRECISION X
        integer N
        ENVJ=0.5D0*DLOG10(6.28D0*N)-N*DLOG10(1.36D0*X/N)
        RETURN
        END

!c  Computation of real spherical harmonics Ylm(theta,phi)
!c
!c  l=0,1,....
!c  m=-l,l
!c
!c  m>0: Y_lm = sqrt(2) ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2  P_l^|m|(cos(theta)) cos(m phi)
!c  m=0: Y_l0 =         ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2  P_l^0  (cos(theta)) 
!c  m<0: Y_lm = sqrt(2) ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2  P_l^|m|(cos(theta)) sin(|m|phi)

!Examples(wikipedia http://en.wikipedia.org/wiki/Table_of_spherical_harmonics#Real_spherical_harmonics)

! l = 0

! Y_00 = \sqrt{1/(4pi)}

! l = 1

! Y_1-1= \sqrt{3/(4pi)} y/r
! Y_10 = \sqrt{3/(4pi)} z/r
! Y_11 = \sqrt{3/(4pi)} x/r
!
! l = 2
!
! Y_2,-2= 1/2 \sqrt{15/pi}  xy/r^2 
! Y_2,-1= 1/2 \sqrt{15/pi}  yz/r^2 
! Y_20  = 1/4 \sqrt{15/pi} (-x^2-y^2 +2z^2)/r^2 
! Y_21  = 1/2 \sqrt{15/pi}  zx/r^2 
! Y_22  = 1/4 \sqrt{15/pi} (x^2-y^2)/r^2 
!
!c
double precision function ylm(l,m,theta,phi)
implicit none
integer l,m
double precision theta,phi,pm,factor,pi,x,fact,sign
DIMENSION PM(0:100,0:100)
pi=dacos(-1.d0)
x=dcos(theta)
sign=(-1.d0)**m
CALL LPMN(100,l,l,X,PM)
factor=dsqrt( (2*l+1)*fact(l-iabs(m)) /(4.d0*pi*fact(l+iabs(m))) )
if(m.gt.0)ylm=sign*dsqrt(2.d0)*factor*pm(m,l)*dcos(dfloat(m)*phi)
if(m.eq.0)ylm=factor*pm(m,l)
if(m.lt.0)ylm=sign*dsqrt(2.d0)*factor*pm(iabs(m),l)*dsin(dfloat(iabs(m))*phi)
end

!c Explicit representation of Legendre polynomials P_n(x) 
!!
!! P_n0(x) = P_n(x)= \sum_{k=0}^n a_k x^k
!!
!!   with  a_k= 2^n  binom(n,k) binom( (n+k-1)/2, n )
!! coef_pm(n,k) is the k_th coefficient of P_n(x)
double precision function coef_pm(n,k)
implicit none
integer n,k
double precision arg,binom,binom_gen
if(n.eq.0.and.k.ne.0)stop 'coef_pm not defined'
if(n.eq.0.and.k.eq.0)then
coef_pm=1.d0
return
endif
arg=0.5d0*dfloat(n+k-1)
coef_pm=2.d0**n*binom(n,k)*binom_gen(arg,n)
end

!! Ylm_bis uses the series expansion of Ylm in xchap^i ychap^j zchap^k
!!  xchap=x/r etc.
!c  m>0: Y_lm = sqrt(2)*factor* P_l^|m|(cos(theta)) cos(m phi)
!c  m=0: Y_l0 =         factor* P_l^0  (cos(theta)) 
!c  m<0: Y_lm = sqrt(2) factor* P_l^|m|(cos(theta)) sin(|m|phi)
!c  factor= ([(2l+1)*(l-|m|)!]/[4pi*(l+|m|)!])^1/2 

!! P_l^m (x) = (-1)**m (1-x**2)^m/2 d^m/dx^m P_l(x)   m >0 or 0
!! the series expansion of P_m (x) is known
!!
!!  sin(theta)**m cos(mphi)  = \sum_0^[m/2] binom(m,2k) x^(m-2k) y^2k (-1)**k   (easy to proove with 
!! Moivre formula)
!! (here x = xchap...)
!!
!!  Ylm m> 0  = \sum_{k=0}^[m/2] \sum_{i=0}^(l-m) c_ki  x^(m-2k) y^2k z^i
!!
!!   c_ki= (-1)^k sqrt(2)*factor*binom(m,2k)*(m+i)!/i!*coef_pm(l,i+m)
!!
!!  Ylm m< 0  = \sum_{k=0}^[(m-1)/2] \sum_{i=0}^(l-m) c_ki  x^(m-(2k+1)) y^(2k+1) z^i
!!
!!   c_ki= (-1)^k sqrt(2)*factor*binom(m,2k+1)*(m+i)!/i!*coef_pm(l,i+m)


double precision function ylm_bis(l,m,theta,phi)
implicit none
integer l,m,k,i
double precision x,y,z,theta,phi,sum,factor,pi,binom,fact,coef_pm,cylm
pi=dacos(-1.d0)
x=dsin(theta)*dcos(phi)
y=dsin(theta)*dsin(phi)
z=dcos(theta)
factor=dsqrt((2*l+1)*fact(l-iabs(m))/(4.d0*pi*fact(l+iabs(m))))
if(m.gt.0)then
sum=0.d0
do k=0,m/2
 do i=0,l-m
  cylm=(-1.d0)**k*factor*dsqrt(2.d0)*binom(m,2*k)*fact(m+i)/fact(i)*coef_pm(l,i+m)
  sum=sum+cylm*x**(m-2*k)*y**(2*k)*z**i
 enddo
enddo
ylm_bis=sum
return
endif
if(m.eq.0)then
sum=0.d0
do i=0,l
 sum=sum+factor*coef_pm(l,i)*z**i
enddo
ylm_bis=sum
return
endif
if(m.lt.0)then
m=-m
sum=0.d0
do k=0,(m-1)/2
 do i=0,l-m
  cylm=(-1.d0)**k*factor*dsqrt(2.d0)*binom(m,2*k+1)*fact(m+i)/fact(i)*coef_pm(l,i+m)
  sum=sum+cylm*x**(m-(2*k+1))*y**(2*k+1)*z**i
 enddo
enddo
ylm_bis=sum
m=-m
return
endif
end

!c
!c  Computation of associated Legendre Polynomials PM(m,n) for n=0,...,N
!c  Here:
!c  n=l (n=0,1,...)  
!c  m=0,1,...,n 
!c  x=cos(theta) 0 < x < 1
!c
!c  This routine computes:   PM(m,n) for n=0,...,N (number N in input) and m=0,..,n
!c   Exemples (see 'Associated Legendre Polynomilas wikipedia')
!c    P_{0}^{0}(x)=1
!c    P_{1}^{-1}(x)=-1/2 P_{1}^{1}(x)
!c    P_{1}^{0}(x)=x
!c    P_{1}^{1}(x)=-(1-x^2)^{1/2}
!c    P_{2}^{-2}(x)=1/24 P_{2}^{2}(x)
!c    P_{2}^{-1}(x)=-1/6 P_{2}^{1}(x)
!c    P_{2}^{0}(x)=1/2 (3x^{2}-1)
!c    P_{2}^{1}(x)=-3x(1-x^2)^{1/2}
!c    P_{2}^{2}(x)=3(1-x^2)
!c
!c Explicit representation:
!!
!! P_n0(x) = P_n(x)= \sum_{k=0}^n a_k x^k
!!
!!   with  a_k= 2^n  binom(n,k) binom( (n+k-1)/2, n )

double precision function binom(i,j)
 implicit none
 integer :: i,j
 double precision :: fact
 binom = fact(i)/(fact(j)*fact(i-j))
end

double precision function binom_gen(alpha,n)
 implicit none
 integer :: n,k
 double precision :: fact,alpha,prod
 prod=1.d0
 do k=0,n-1
   prod=prod*(alpha-k)
   binom_gen = prod/(fact(n))
 enddo
end

double precision function test_int(g_a,g_b,g_c,ac,bc)
implicit none
double precision factor,g_a,g_b,g_c,ac,bc,x,dx,sum,alpha,beta,pi
integer i,npts
pi=dacos(-1.d0)
factor=0.5d0*pi/(g_a*g_b*ac*bc*dsqrt(g_a+g_b+g_c))*dexp(-g_a*ac**2-g_b*bc**2)
npts=2000
dx=20.d0/npts
sum=0.d0
alpha=(2.d0*g_a*ac+2.d0*g_b*bc)/dsqrt(g_c+g_a+g_b)
beta=(2.d0*g_b*bc-2.d0*g_b*bc)/dsqrt(g_c+g_a+g_b)
do i=1,npts
 x=(i-1)*dx+0.5d0*dx
 sum=sum+dx*dexp(-x**2)*(dcosh(alpha*x)-dcosh(beta*x))
enddo
test_int=factor*sum
end

recursive function fact1(n,a) result(x)
implicit none
integer n
double precision a,x,erf
if(n.eq.0)then
x=dsqrt(dacos(-1.d0))/2.d0*erf(a)
return
endif
if(n.eq.1)then
x=1.d0-dexp(-a**2)
return
endif
if(mod(n,2).eq.0)x=0.5d0*dfloat(n-1)*fact1(n-2,a)+a**n*dexp(-a**2)
if(mod(n,2).eq.1)x=0.5d0*dfloat(n-1)*fact1(n-2,a)+0.5d0*a**(n-1)*dexp(-a**2)
end

      double precision FUNCTION ERF(X)
      implicit double precision(a-h,o-z)
      IF(X.LT.0.d0)THEN
        ERF=-GAMMP(.5d0,X**2)
      ELSE
        ERF=GAMMP(.5d0,X**2)
      ENDIF
      RETURN
      END

      double precision function coef_nk(n,k)
      implicit none
      integer n,k
      double precision gam,dblefact,fact
      gam=dblefact(2*(n+k)+1)
      coef_nk=1.d0/(2.d0**k*fact(k)*gam)
      end

!!    Calculation of
!!
!!    I= \int dx x**l *exp(-gam*x**2) M_n(ax) M_m(bx)
!!
!!    M_n(x) modified spherical bessel function 
!!
      double precision function int_prod_bessel(l,gam,n,m,a,b)
      implicit none
      integer n,k,m,q,l,kcp
      double precision gam,dblefact,fact,pi,a,b
      double precision int,intold,sum,coef_nk,crochet
      logical done

      if(a.ne.0.d0.and.b.ne.0.d0)then
       q=0
       intold=-1.d0
       int=0.d0
       done=.false.
       kcp=0
       do while (.not.done)  
        kcp=kcp+1
        sum=0.d0
        do k=0,q
         sum=sum+coef_nk(n,k)*coef_nk(m,q-k)*a**(n+2*k)*b**(m-2*k+2*q)
        enddo
        int=int+sum*crochet(2*q+n+m+l,gam)
        if(dabs(int-intold).lt.1d-15)then
         done=.true.
        else
         q=q+1
         intold=int
        endif
       enddo
       int_prod_bessel=int
       if(kcp.gt.100) then
          print*,"l",l
          print*, "gam", gam
          print*, "n", n
          print*, "m", m
          print*, "a", a
          print*, "b", b
          print*, "kcp", kcp
          print*,'**WARNING** bad convergence in int_prod_bessel'
        endif
       return
      endif

      if(a.eq.0.d0.and.b.ne.0.d0)then
       if(n.ne.0)then
        int_prod_bessel=0.d0
        return
       endif
       q=0
       intold=-1.d0
       int=0.d0
       done=.false.
       kcp=0
       do while (.not.done)
        kcp=kcp+1
        int=int+coef_nk(m,q)*b**(m+2*q)*crochet(2*q+m+l,gam)
        if(dabs(int-intold).lt.1d-15)then
         done=.true.
        else
         q=q+1
         intold=int
        endif
       enddo
       int_prod_bessel=int
       if(kcp.gt.100)stop '**WARNING** bad convergence in int_prod_bessel'
       return
      endif

      if(a.ne.0.d0.and.b.eq.0.d0)then
       if(m.ne.0)then
        int_prod_bessel=0.d0
        return
       endif
       q=0
       intold=-1.d0
       int=0.d0
       done=.false.
       kcp=0
       do while (.not.done)
        kcp=kcp+1
        int=int+coef_nk(n,q)*a**(n+2*q)*crochet(2*q+n+l,gam)
        if(dabs(int-intold).lt.1d-15)then
         done=.true.
        else
         q=q+1
         intold=int
        endif
       enddo
       int_prod_bessel=int
       if(kcp.gt.100)stop '**WARNING** bad convergence in int_prod_bessel'
       return
       endif

      if(a.eq.0.d0.and.b.eq.0.d0)then
       if(n.ne.0.or.m.ne.0)then
        int_prod_bessel=0.d0
        return
       endif
       int_prod_bessel=crochet(l,gam)
       return
      endif

      stop 'pb in int_prod_bessel!!'
      end

!!    Calculation of
!!
!!    I= \int dx x**l *exp(-gam*x**2) M_n(ax)
!!
!!    M_n(x) modified spherical bessel function 
!!
      double precision function int_prod_bessel_loc(l,gam,n,a)
      implicit none
      integer n,k,l,kcp
      double precision gam,a
      double precision int,intold,coef_nk,crochet
      logical done
      k=0
      intold=-1.d0
      int=0.d0
      done=.false.
      kcp=0
      do while (.not.done)
       kcp=kcp+1
       int=int+coef_nk(n,k)*a**(n+2*k)*crochet(2*k+n+l,gam)
       if(dabs(int-intold).lt.1d-15)then
        done=.true.
       else
        k=k+1
        intold=int
       endif
      enddo
      int_prod_bessel_loc=int
      if(kcp.gt.100)print*,'**WARNING** bad convergence in int_prod_bessel'
      end

      double precision function int_prod_bessel_num(l,gam,n,m,a,b)
      implicit none
      integer n,m,l,i,npoints
      double precision gam,a,b
      double precision sum,dx,x,bessel_mod
      sum=0.d0
      npoints=20000
      dx=30.d0/npoints
      do i=1,npoints
       x=(i-1)*dx+0.5d0*dx
       sum=sum+dx*x**l*dexp(-gam*x**2)*bessel_mod(a*x,n)*bessel_mod(b*x,m)
      enddo
      int_prod_bessel_num=sum
      end


      
