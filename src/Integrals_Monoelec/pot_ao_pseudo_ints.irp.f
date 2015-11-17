BEGIN_PROVIDER [ double precision, ao_pseudo_integral, (ao_num_align,ao_num)]
  implicit none
  BEGIN_DOC
! Pseudo-potential
  END_DOC
  if (do_pseudo) then
    ao_pseudo_integral = ao_pseudo_integral_local + ao_pseudo_integral_non_local 
  else
    ao_pseudo_integral = 0.d0
  endif
END_PROVIDER

BEGIN_PROVIDER [ double precision, ao_pseudo_integral_local, (ao_num_align,ao_num)]
  implicit none
  BEGIN_DOC
  ! Local pseudo-potential
  END_DOC
  double precision               :: alpha, beta, gama, delta
  integer                        :: num_A,num_B
  double precision               :: A_center(3),B_center(3),C_center(3)
  integer                        :: power_A(3),power_B(3)
  integer                        :: i,j,k,l,n_pt_in,m
  double precision               :: Vloc, Vpseudo
  
  double precision               :: cpu_1, cpu_2, wall_1, wall_2, wall_0
  integer                        :: thread_num
  !$ integer                     :: omp_get_thread_num
  
  ao_pseudo_integral_local = 0.d0
  
  !! Dump array
  integer, allocatable           :: n_k_dump(:)
  double precision, allocatable  :: v_k_dump(:), dz_k_dump(:)
  
  allocate(n_k_dump(1:pseudo_klocmax), v_k_dump(1:pseudo_klocmax), dz_k_dump(1:pseudo_klocmax))
  
  print*, 'Providing the nuclear electron pseudo integrals (local)'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  
  thread_num = 0
  !$OMP PARALLEL                                                     &
      !$OMP DEFAULT (NONE)                                           &
      !$OMP PRIVATE (i,j,k,l,m,alpha,beta,A_center,B_center,C_center,power_A,power_B,&
      !$OMP          num_A,num_B,Z,c,n_pt_in,                        &
      !$OMP          v_k_dump,n_k_dump, dz_k_dump,                   &
      !$OMP          wall_0,wall_2,thread_num)                       &
      !$OMP SHARED (ao_num,ao_prim_num,ao_expo_ordered_transp,ao_power,ao_nucl,nucl_coord,ao_coef_normalized_ordered_transp,&
      !$OMP         ao_pseudo_integral_local,nucl_num,nucl_charge,   &
      !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_v_k,pseudo_n_k, pseudo_dz_k,&
      !$OMP         wall_1)
  
  !$ thread_num = omp_get_thread_num()
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
          double precision               :: c
          c = 0.d0
          
          if (dabs(ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i))&
                < 1.d-10) then
            cycle
          endif
          do  k = 1, nucl_num
            double precision               :: Z
            Z = nucl_charge(k)
            
            C_center(1:3) = nucl_coord(k,1:3)
            
            v_k_dump = pseudo_v_k(k,1:pseudo_klocmax)
            n_k_dump = pseudo_n_k(k,1:pseudo_klocmax)
            dz_k_dump =  pseudo_dz_k(k,1:pseudo_klocmax)
            
            c = c + Vloc(pseudo_klocmax, v_k_dump,n_k_dump, dz_k_dump,&
                A_center,power_A,alpha,B_center,power_B,beta,C_center)
            
          enddo
          ao_pseudo_integral_local(i,j) = ao_pseudo_integral_local(i,j) +&
              ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i)*c
        enddo
      enddo
    enddo
    
    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(ao_num), '% in ',                &
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
  double precision               :: alpha, beta, gama, delta
  integer                        :: num_A,num_B
  double precision               :: A_center(3),B_center(3),C_center(3)
  integer                        :: power_A(3),power_B(3)
  integer                        :: i,j,k,l,n_pt_in,m
  double precision               :: Vloc, Vpseudo
  !$ integer                     :: omp_get_thread_num
  
  double precision               :: cpu_1, cpu_2, wall_1, wall_2, wall_0
  integer                        :: thread_num
  
  ao_pseudo_integral_non_local = 0.d0
  
  !! Dump array
  integer, allocatable           :: n_kl_dump(:,:)
  double precision, allocatable  :: v_kl_dump(:,:), dz_kl_dump(:,:)
  
  allocate(n_kl_dump(pseudo_kmax,0:pseudo_lmax), v_kl_dump(pseudo_kmax,0:pseudo_lmax), dz_kl_dump(pseudo_kmax,0:pseudo_lmax))
  
  print*, 'Providing the nuclear electron pseudo integrals (non-local)'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  thread_num = 0
  !$OMP PARALLEL                                                     &
      !$OMP DEFAULT (NONE)                                           &
      !$OMP PRIVATE (i,j,k,l,m,alpha,beta,A_center,B_center,C_center,power_A,power_B,&
      !$OMP          num_A,num_B,Z,c,n_pt_in,                        &
      !$OMP          n_kl_dump, v_kl_dump, dz_kl_dump,               &
      !$OMP          wall_0,wall_2,thread_num)                       &
      !$OMP SHARED (ao_num,ao_prim_num,ao_expo_ordered_transp,ao_power,ao_nucl,nucl_coord,ao_coef_normalized_ordered_transp,&
      !$OMP         ao_pseudo_integral_non_local,nucl_num,nucl_charge,&
      !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_n_kl, pseudo_v_kl, pseudo_dz_kl,&
      !$OMP         wall_1)
  
  !$ thread_num = omp_get_thread_num()
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
          double precision               :: c
          c = 0.d0
          
          if (dabs(ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i))&
                < 1.d-10) then
            cycle
          endif
          
          do  k = 1, nucl_num
            double precision               :: Z
            Z = nucl_charge(k)
            
            C_center(1:3) = nucl_coord(k,1:3)
            
            n_kl_dump = pseudo_n_kl(k,1:pseudo_kmax,0:pseudo_lmax)
            v_kl_dump = pseudo_v_kl(k,1:pseudo_kmax,0:pseudo_lmax)
            dz_kl_dump = pseudo_dz_kl(k,1:pseudo_kmax,0:pseudo_lmax)
            
            c = c + Vpseudo(pseudo_lmax,pseudo_kmax,v_kl_dump,n_kl_dump,dz_kl_dump,A_center,power_A,alpha,B_center,power_B,beta,C_center)
            
          enddo
          ao_pseudo_integral_non_local(i,j) = ao_pseudo_integral_non_local(i,j) +&
              ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i)*c
        enddo
      enddo
    enddo
    
    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(j)/float(ao_num), '% in ',                &
            wall_2-wall_1, 's'
      endif
    endif
  enddo
  
  !$OMP END DO
  !$OMP END PARALLEL
  
  
  deallocate(n_kl_dump,v_kl_dump, dz_kl_dump)
  
  
END_PROVIDER



