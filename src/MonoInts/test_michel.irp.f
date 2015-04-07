!!
!!  Computation of Vps, matrix element of the 
!!  pseudo-potential centered at point C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Vps= < Phi_A | Vloc(C) + Vpp(C) | Phi_B>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Phi_M (M=A,B) Cartesian gaussian orbital centered at point M :
!!  Phi_M = (x-M_x)**n^M_x *(y-M_y)**n^M_y *(z-M_z)**n^M_z exp(-g_M rM**2)
!!         with rM**2=(x-M_x)**2 + (y-M_y)**2 + (z-M_z)**2
!!
!!**  Vloc(C)= \sum_{k=1}^klocmax v_k rC**n_k exp(-dz_k rC**2)
!!
!!**  Vpp(C)=  \sum_{l=0}^lmax v_l(rC)  \sum_{m=-l}^{m=l} |Y_lm> <Y_lm|
!!
!!              v_l(rC) = \sum_{k=1}^kmax v_kl rC**n_kl exp(-dz_kl rC**2)
!!
!!              Y_lm : real spherical harmonics:
!!                   Y_00  = sqrt(1/4pi)
!!                   Y_11  = sqrt(3/4pi) xchap
!!                   Y_10  = sqrt(3/4pi) ychap
!!                   Y_1-1 = sqrt(3/4pi) zchap
!!                   Y_22  = sqrt(15/16pi) (xchap**2-ychap**2)
!!                   Y_21  = sqrt(15/4pi)  xchap*zchap
!!                   Y_20  = sqrt(15/16pi)(-xchap**2-ychap**2+ 2 zchap**2)
!!                   Y_2-1 = sqrt(15/4pi) ychap*zchap
!!                   Y_2-2 = sqrt(15/4pi) xchap*ychap
!!                      etc.
!!                            xchap=x/r ychap=y/r  zchap=z/r
!!
!! Routine computing  <Phi_A|Vpp(C)|Phi_B> :
!! function Vpseudo(lmax,kmax,v_kl,n_kl,dz_kl,a,n_a,g_a,b,n_b,g_b,c)
!!  lmax of formula above
!!  kmax of formula above
!!    v_kl = array v_kl(kmax_max,0:lmax_max)
!!    n_kl = array n_kl(kmax_max,0:lmax_max)
!!    dz_kl = array dz_kl(kmax_max,0:lmax_max)
!!    n_a(1),n_a(2),n_a(3)
!!    a(1),a(2),a(3)
!!    g_a
!!    n_b(1),n_b(2),n_b(3)
!!    b(1),b(2),b(3)
!!    g_b
!!    c(1),c(2),c(3)
!!
!! Routine computing  <Phi_A|Vloc(C)|Phi_B> :
!! function Vloc(klocmax,v_k,n_k,dz_k,a,n_a,g_a,b,n_b,g_b,c)
!!  klocmax of formula above
!!  v_k = array v_k(klocmax_max)
!!  n_k = array n_k(klocmax_max)
!!  dz_k= array dz_k(klocmax_max)
!! Routine total matrix element <Phi_A|Vloc(C)+Vlpp(C)|Phi_B> :
!! function Vps(a,n_a,g_a,b,n_b,g_b,c,klocmax,v_k,n_k,dz_k,lmax,kmax,v_kl,n_kl,dz_kl)
!!
!! Routines Vps_num, Vpseudo_num, and Vloc_num =  brute force numerical 
!! estimations of the same integrals


program compute_integrals_pseudo
  implicit none
  integer n_a(3),n_b(3),npts
  double precision g_a,g_b,a(3),b(3),c(3)
  double precision Vpseudo,Vpseudo_num,Vloc,Vloc_num
  double precision v3,v4
  

  double precision vps,vps_num
  
  ! PSEUDOS
  integer nptsgridmax,nptsgrid
  double precision coefs_pseudo,ptsgrid
  
  double precision rmax
  double precision time_1,time_2,time_3,time_4,time_5
  integer kga,kgb,na1,na2,na3,nb1,nb2,nb3
  
  CALL RANDOM_SEED()
  
  nptsgrid=50
  call initpseudos(nptsgrid)
  
  PROVIDE ezfio_filename

  !                 
  ! |   _   _  _. | 
  ! |_ (_) (_ (_| | 
  !                 

  integer klocmax
  integer, allocatable ::  n_k(:)
  double precision, allocatable ::  v_k(:), dz_k(:)

  call ezfio_get_pseudo_klocmax(klocmax)

  allocate(n_k(klocmax),v_k(klocmax), dz_k(klocmax))

  call ezfio_get_pseudo_v_k(v_k)
  call ezfio_get_pseudo_n_k(n_k)
  call ezfio_get_pseudo_dz_k(dz_k)

  print*, "klocmax", klocmax

  print*, "n_k_ezfio", n_k
  print*, "v_k_ezfio",v_k
  print*, "dz_k_ezfio", dz_k

  !                               
  ! |\ |  _  ._    |  _   _  _. | 
  ! | \| (_) | |   | (_) (_ (_| | 
  !                              

  !! Parameters of non local part of pseudo:

  integer :: kmax,lmax
  integer, allocatable ::  n_kl(:,:)
  double precision, allocatable ::  v_kl(:,:), dz_kl(:,:) 

  call ezfio_get_pseudo_lmax(lmax)
  call ezfio_get_pseudo_kmax(kmax)
  lmax = lmax - 1 

  allocate(n_kl(kmax,0:lmax), v_kl(kmax,0:lmax), dz_kl(kmax,0:lmax)) 

  call ezfio_get_pseudo_n_kl(n_kl)
  call ezfio_get_pseudo_v_kl(v_kl)
  call ezfio_get_pseudo_dz_kl(dz_kl)


  print*, "lmax",lmax
  print*, "kmax", kmax

  print*,"n_kl_ezfio", n_kl
  print*,"v_kl_ezfio", v_kl
  print*,"dz_kl_ezfio", dz_kl

  !  _                
  ! /   _. |  _     | 
  ! \_ (_| | (_ |_| | 
  !                   

  write(*,*)'a?'
  read*,a(1),a(2),a(3)
  !write(*,*)'b?'
  !read*,b(1),b(2),b(3)
  b(1)=-0.1d0
  b(2)=-0.2d0
  b(3)=0.3d0
  !write(*,*)'a?'
  !read*,c(1),c(2),c(3)
  c(1)=0.1d0
  c(2)=0.2d0
  c(3)=0.3d0
  
  print*,'ntps? rmax for brute force integration'
  read*,npts,rmax
  
  do kga=0,5
    g_a=10.d0**kga
  do kgb=0,5
    g_b=10.d0**kgb
    
    do na1=0,1
    do na2=0,1
    do na3=0,1
    do nb1=0,1
    do nb2=0,1
    do nb3=0,1
      n_a(1)=na1
      n_a(2)=na2
      n_a(3)=na3
      n_b(1)=nb1
      n_b(2)=nb2
      n_b(3)=nb3
      
      v3=Vps(a,n_a,g_a,b,n_b,g_b,c,klocmax,v_k,n_k,dz_k,lmax,kmax,v_kl,n_kl,dz_kl)
      v4=Vps_num(npts,rmax,a,n_a,g_a,b,n_b,g_b,c,klocmax,v_k,n_k,dz_k,lmax,kmax,v_kl,n_kl,dz_kl)
      print*,'Vps= ',v3,' Vps_num=',v4,' diff=',v4-v3
      write(33,'(3f10.6)')v3,v4,v4-v3
      
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
  enddo
  enddo
  
end
