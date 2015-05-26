BEGIN_PROVIDER [ double precision, aux_pseudo_integral, (aux_basis_num_sqrt,aux_basis_num_sqrt)]
  implicit none
  BEGIN_DOC
! Pseudo-potential
  END_DOC
  if (do_pseudo) then
!    aux_pseudo_integral = aux_pseudo_integral_local + aux_pseudo_integral_non_local 
!    aux_pseudo_integral = aux_pseudo_integral_local 
    aux_pseudo_integral = aux_pseudo_integral_non_local 
  else
    aux_pseudo_integral = 0.d0
  endif
END_PROVIDER

 BEGIN_PROVIDER [ double precision, aux_pseudo_integral_local, (aux_basis_num_sqrt,aux_basis_num_sqrt)]
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

  aux_pseudo_integral_local = 0.d0

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
  !$OMP SHARED (aux_basis_num_sqrt,aux_basis_prim_num,aux_basis_expo_transp,aux_basis_power,aux_basis_nucl,nucl_coord,aux_basis_coef_transp, &
  !$OMP         aux_pseudo_integral_local,nucl_num,nucl_charge, &
  !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_v_k,pseudo_n_k, pseudo_dz_k, &
  !$OMP         wall_1)
  
  !$OMP DO SCHEDULE (guided)

  do j = 1, aux_basis_num_sqrt

   num_A = aux_basis_nucl(j)
   power_A(1:3)= aux_basis_power(j,1:3)
   A_center(1:3) = nucl_coord(num_A,1:3)

  do i = 1, aux_basis_num_sqrt

   num_B = aux_basis_nucl(i)
   power_B(1:3)= aux_basis_power(i,1:3)
   B_center(1:3) = nucl_coord(num_B,1:3)

    do l=1,aux_basis_prim_num(j)
     alpha = aux_basis_expo_transp(l,j)
  
    do m=1,aux_basis_prim_num(i)
      beta = aux_basis_expo_transp(m,i)
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
      aux_pseudo_integral_local(i,j) = aux_pseudo_integral_local(i,j) + &
                                   aux_basis_coef_transp(l,j)*aux_basis_coef_transp(m,i)*c
     enddo
     enddo
  enddo

    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(aux_basis_num_sqrt), '% in ',  &
                                 wall_2-wall_1, 's'
      endif
    endif
  enddo

 !$OMP END DO
 !$OMP END PARALLEL


  deallocate(n_k_dump,v_k_dump, dz_k_dump)

 END_PROVIDER


 BEGIN_PROVIDER [ double precision, aux_pseudo_integral_non_local, (aux_basis_num_sqrt,aux_basis_num_sqrt)]
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

  aux_pseudo_integral_non_local = 0.d0

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
  !$OMP SHARED (aux_basis_num_sqrt,aux_basis_prim_num,aux_basis_expo_transp,aux_basis_power,aux_basis_nucl,nucl_coord,aux_basis_coef_transp, &
  !$OMP         aux_pseudo_integral_non_local,nucl_num,nucl_charge, &
  !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_n_kl, pseudo_v_kl, pseudo_dz_kl, &
  !$OMP         wall_1)
  
  !$OMP DO SCHEDULE (guided)

  do j = 1, aux_basis_num_sqrt

   num_A = aux_basis_nucl(j)
   power_A(1:3)= aux_basis_power(j,1:3)
   A_center(1:3) = nucl_coord(num_A,1:3)

  do i = 1, aux_basis_num_sqrt

   num_B = aux_basis_nucl(i)
   power_B(1:3)= aux_basis_power(i,1:3)
   B_center(1:3) = nucl_coord(num_B,1:3)

    do l=1,aux_basis_prim_num(j)
     alpha = aux_basis_expo_transp(l,j)
  
    do m=1,aux_basis_prim_num(i)
      beta = aux_basis_expo_transp(m,i)
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
      aux_pseudo_integral_non_local(i,j) = aux_pseudo_integral_non_local(i,j) + &
                                   aux_basis_coef_transp(l,j)*aux_basis_coef_transp(m,i)*c
     enddo
     enddo
  enddo

    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(aux_basis_num_sqrt), '% in ',  &
                                 wall_2-wall_1, 's'
      endif
    endif
  enddo

 !$OMP END DO
 !$OMP END PARALLEL


  deallocate(n_kl_dump,v_kl_dump, dz_kl_dump)


END_PROVIDER

BEGIN_PROVIDER [ double precision, ao_pseudo_grid, (ao_num,-pseudo_lmax:pseudo_lmax,0:pseudo_lmax,nucl_num,pseudo_grid_size)  ]
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
 double precision :: y

 dr = pseudo_grid_rmax/dble(pseudo_grid_size)
 r(1) = 0.d0
 do j=2,pseudo_grid_size
   r(j) = r(j-1) + dr
 enddo

 ao_pseudo_grid = 0.d0
 do j=1,pseudo_grid_size
   do k=1,nucl_num
     c(1:3) = nucl_coord(k,1:3)
     do l=0,pseudo_lmax
       do i=1,ao_num
         a(1:3) = nucl_coord(ao_nucl(i),1:3)
         n_a(1:3) = ao_power(i,1:3)
         do p=1,ao_prim_num(i)
           g_a = ao_expo_ordered_transp(p,i)
           do m=-l,l
             y = ylm_orb(l,m,c,a,n_a,g_a,r(j))
             ao_pseudo_grid(i,m,l,k,j) = ao_pseudo_grid(i,m,l,k,j) + &
                 ao_coef_normalized_ordered_transp(p,i)*y
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo

END_PROVIDER


BEGIN_PROVIDER [ double precision, mo_pseudo_grid, (mo_tot_num,-pseudo_lmax:pseudo_lmax,0:pseudo_lmax,nucl_num,pseudo_grid_size)  ]
 implicit none
 BEGIN_DOC
! Grid points for f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \phi_i^{A} (r-r_A) d\Omega_C
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
 double precision :: y

 dr = pseudo_grid_rmax/dble(pseudo_grid_size)
 r(1) = 0.d0
 do j=2,pseudo_grid_size
   r(j) = r(j-1) + dr
 enddo

 mo_pseudo_grid = 0.d0
 do n=1,pseudo_grid_size
   do k=1,nucl_num
     do l=0,pseudo_lmax
       do m=-l,l
         do j=1,mo_tot_num
           do i=1,ao_num
!             mo_pseudo_grid(
!         ao_pseudo_grid(j,i,m,l,k) = ao_pseudo_grid(j,i,m,l,k) +           &
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo

END_PROVIDER

double precision function test_pseudo_grid(i,j)
  implicit none
  integer, intent(in) :: i,j
  integer :: k,l,m,n
  double precision :: r, dr,u
  dr = pseudo_grid_rmax/dble(pseudo_grid_size)

  test_pseudo_grid = 0.d0
  r = 0.d0
  do k=1,pseudo_grid_size
    do n=1,nucl_num
      do l = 0,pseudo_lmax
        u = pseudo_v_kl(n,l,1) * exp(-pseudo_dz_kl(n,l,1)*r*r)* r*r*dr
        do m=-l,l
          test_pseudo_grid +=  ao_pseudo_grid(i,m,l,n,k) * ao_pseudo_grid(j,m,l,n,k) * u 
        enddo
      enddo
    enddo
    r = r+dr
  enddo
end
!
