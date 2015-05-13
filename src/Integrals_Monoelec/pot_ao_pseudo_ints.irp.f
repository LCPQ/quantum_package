BEGIN_PROVIDER [ double precision, ao_pseudo_integral, (ao_num_align,ao_num)]
  implicit none
  BEGIN_DOC
! Pseudo-potential
  END_DOC
  if (do_pseudo) then
    ao_pseudo_integral = ao_pseudo_integral_local !+ ao_pseudo_integral_non_local 
  else
    ao_pseudo_integral = 0.d0
  endif
END_PROVIDER

 BEGIN_PROVIDER [ double precision, ao_pseudo_integral_local, (ao_num_align,ao_num)]
  implicit none
  BEGIN_DOC
! Local pseudo-potential
  END_DOC
 double precision  :: alpha, beta, gama, delta
 integer           :: num_A,num_B
 double precision  :: A_center(3),B_center(3),C_center(3)
 integer           :: power_A(3),power_B(3)
 integer           :: i,j,k,l,n_pt_in,m
 double precision  ::  Vloc, Vpseudo

 double precision  :: cpu_1, cpu_2, wall_1, wall_2, wall_0
 integer           :: thread_num

  ao_pseudo_integral_local = 0.d0

  !! Dump array 
  integer, allocatable ::  n_k_dump(:)
  double precision, allocatable ::  v_k_dump(:), dz_k_dump(:) 
 
  allocate(n_k_dump(1:pseudo_klocmax), v_k_dump(1:pseudo_klocmax), dz_k_dump(1:pseudo_klocmax)) 


  !  _                
  ! /   _. |  _     | 
  ! \_ (_| | (_ |_| | 
  !                   

  print*, 'Providing the nuclear electron pseudo integrals '

  call wall_time(wall_1)
  call cpu_time(cpu_1)

  !$OMP PARALLEL &
  !$OMP DEFAULT (NONE) &
  !$OMP PRIVATE (i,j,k,l,m,alpha,beta,A_center,B_center,C_center,power_A,power_B, &
  !$OMP          num_A,num_B,Z,c,n_pt_in, &
  !$OMP          v_k_dump,n_k_dump, dz_k_dump, &
  !$OMP          wall_0,wall_2,thread_num) & 
  !$OMP SHARED (ao_num,ao_prim_num,ao_expo_ordered_transp,ao_power,ao_nucl,nucl_coord,ao_coef_normalized_ordered_transp, &
  !$OMP         ao_pseudo_integral_local,nucl_num,nucl_charge, &
  !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_v_k,pseudo_n_k, pseudo_dz_k, &
  !$OMP         wall_1)
  
  !$OMP DO SCHEDULE (guided)

  do j = 1, ao_num

   num_A = ao_nucl(j)
   power_A(1:3)= ao_power(j,1:3)
   A_center(1:3) = nucl_coord(num_A,1:3)

  do i = 1, ao_num

   num_B = ao_nucl(i)
   power_B(1:3)= ao_power(i,1:3)
   B_center(1:3) = nucl_coord(num_B,1:3)

    do l=1,ao_prim_num(j)
     alpha = ao_expo_ordered_transp(l,j)
  
    do m=1,ao_prim_num(i)
      beta = ao_expo_ordered_transp(m,i)
      double precision :: c
      c = 0.d0
       
      do  k = 1, nucl_num
        double precision :: Z
        Z = nucl_charge(k)
  
        C_center(1:3) = nucl_coord(k,1:3)
  
        v_k_dump = pseudo_v_k(k,1:pseudo_klocmax)
        n_k_dump = pseudo_n_k(k,1:pseudo_klocmax)
        dz_k_dump =  pseudo_dz_k(k,1:pseudo_klocmax)

        c = c + Vloc(pseudo_klocmax, v_k_dump,n_k_dump, dz_k_dump, &
                     A_center,power_A,alpha,B_center,power_B,beta,C_center)
  
      enddo
      ao_pseudo_integral_local(i,j) = ao_pseudo_integral_local(i,j) + &
                                   ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i)*c
     enddo
     enddo
  enddo

    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(ao_num), '% in ',  &
                                 wall_2-wall_1, 's'
      endif
    endif
  enddo

 !$OMP END DO
 !$OMP END PARALLEL


  deallocate(n_k_dump,v_k_dump, dz_k_dump)

 END_PROVIDER


 BEGIN_PROVIDER [ double precision, ao_pseudo_integral_non_local, (ao_num_align,ao_num)]
  implicit none
  BEGIN_DOC
! Local pseudo-potential
  END_DOC
 double precision  :: alpha, beta, gama, delta
 integer           :: num_A,num_B
 double precision  :: A_center(3),B_center(3),C_center(3)
 integer           :: power_A(3),power_B(3)
 integer           :: i,j,k,l,n_pt_in,m
 double precision  ::  Vloc, Vpseudo

 double precision  :: cpu_1, cpu_2, wall_1, wall_2, wall_0
 integer           :: thread_num

  ao_pseudo_integral_non_local = 0.d0

  !! Dump array 
  integer, allocatable ::  n_kl_dump(:,:)
  double precision, allocatable ::  v_kl_dump(:,:), dz_kl_dump(:,:) 
 
  allocate(n_kl_dump(pseudo_kmax,0:pseudo_lmax), v_kl_dump(pseudo_kmax,0:pseudo_lmax), dz_kl_dump(pseudo_kmax,0:pseudo_lmax)) 

  !  _                
  ! /   _. |  _     | 
  ! \_ (_| | (_ |_| | 
  !                   

  print*, 'Providing the nuclear electron pseudo integrals '

  call wall_time(wall_1)
  call cpu_time(cpu_1)

  !$OMP PARALLEL &
  !$OMP DEFAULT (NONE) &
  !$OMP PRIVATE (i,j,k,l,m,alpha,beta,A_center,B_center,C_center,power_A,power_B, &
  !$OMP          num_A,num_B,Z,c,n_pt_in, &
  !$OMP          n_kl_dump, v_kl_dump, dz_kl_dump, &
  !$OMP          wall_0,wall_2,thread_num) & 
  !$OMP SHARED (ao_num,ao_prim_num,ao_expo_ordered_transp,ao_power,ao_nucl,nucl_coord,ao_coef_normalized_ordered_transp, &
  !$OMP         ao_pseudo_integral_non_local,nucl_num,nucl_charge, &
  !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_n_kl, pseudo_v_kl, pseudo_dz_kl, &
  !$OMP         wall_1)
  
  !$OMP DO SCHEDULE (guided)

  do j = 1, ao_num

   num_A = ao_nucl(j)
   power_A(1:3)= ao_power(j,1:3)
   A_center(1:3) = nucl_coord(num_A,1:3)

  do i = 1, ao_num

   num_B = ao_nucl(i)
   power_B(1:3)= ao_power(i,1:3)
   B_center(1:3) = nucl_coord(num_B,1:3)

    do l=1,ao_prim_num(j)
     alpha = ao_expo_ordered_transp(l,j)
  
    do m=1,ao_prim_num(i)
      beta = ao_expo_ordered_transp(m,i)
      double precision :: c
      c = 0.d0
       
      do  k = 1, nucl_num
        double precision :: Z
        Z = nucl_charge(k)
  
        C_center(1:3) = nucl_coord(k,1:3)
  
        n_kl_dump = pseudo_n_kl(k,1:pseudo_kmax,0:pseudo_lmax)
        v_kl_dump = pseudo_v_kl(k,1:pseudo_kmax,0:pseudo_lmax)
        dz_kl_dump = pseudo_dz_kl(k,1:pseudo_kmax,0:pseudo_lmax)

        c = c + Vpseudo(pseudo_lmax,pseudo_kmax,v_kl_dump,n_kl_dump,dz_kl_dump,A_center,power_A,alpha,B_center,power_B,beta,C_center)
  
      enddo
      ao_pseudo_integral_non_local(i,j) = ao_pseudo_integral_non_local(i,j) + &
                                   ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i)*c
     enddo
     enddo
  enddo

    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(ao_num), '% in ',  &
                                 wall_2-wall_1, 's'
      endif
    endif
  enddo

 !$OMP END DO
 !$OMP END PARALLEL


  deallocate(n_kl_dump,v_kl_dump, dz_kl_dump)


 END_PROVIDER

BEGIN_PROVIDER [ double precision, pseudo_grid, (pseudo_grid_size,ao_num,-pseudo_lmax:pseudo_lmax,0:pseudo_lmax,nucl_num)  ]
 implicit none
 BEGIN_DOC
! Grid points for f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \chi_i^{A} (r-r_A) d\Omega_C
!
! <img src="http://latex.codecogs.com/gif.latex?f(|r-r_A|)&space;=&space;\int&space;Y_{lm}^{C}&space;(|r-r_C|,&space;\Omega_C)&space;\chi_i^{A}&space;(r-r_A)&space;d\Omega_C"
! title="f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \chi_i^{A} (r-r_A) d\Omega_C" />
 END_DOC
 ! l,m    : Y(l,m) parameters
 ! c(3)   : pseudopotential center
 ! a(3)   : Atomic Orbital center
 ! n_a(3) : Powers of x,y,z in the Atomic Orbital
 ! g_a    : Atomic Orbital exponent      
 ! r      : Distance between the Atomic Orbital center and the considered point
 double precision, external :: ylm_orb
 integer :: n_a(3)
 double precision :: a(3), c(3), g_a
 integer :: i,j,k,l,m,n,p
 double precision :: r(pseudo_grid_size), dr, Ulc
 double precision, parameter :: rmax= 10.d0

 dr = rmax/dble(pseudo_grid_size)
 r(1) = 0.d0
 do j=2,pseudo_grid_size
   r(j) = r(j-1) + dr
 enddo

 pseudo_grid = 0.d0
 do k=1,nucl_num
   c(1:3) = nucl_coord(k,1:3)
   do l=0,pseudo_lmax
     do i=1,ao_num
       a(1:3) = nucl_coord(ao_nucl(i),1:3)
       n_a(1:3) = ao_power(i,1:3)
       do j=1,pseudo_grid_size
         do p=1,ao_prim_num(i)
           g_a = ao_expo_ordered_transp(p,i)
           do m=-l,l
             double precision               :: y
             !            y = ylm_orb(l,m,c,a,n_a,g_a,r(j))
             !            if (y /= 0.d0) then
             !              print *, 'y = ', y
             !              print *, 'l = ', l
             !              print *, 'm = ', m
             !              print *, 'c = ', c
             !              print *, 'a = ', a
             !              print *, 'n = ', n_a
             !              print *, 'g = ', g_a
             !              print *, 'r = ', r(j)
             !              print *,  ''
             !            endif
             y = ylm_orb(l,m,c,a,n_a,g_a,r(j))
             pseudo_grid(j,i,m,l,k) = pseudo_grid(j,i,m,l,k) +           &
                 ao_coef_normalized_ordered_transp(p,i)*y
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo

END_PROVIDER


