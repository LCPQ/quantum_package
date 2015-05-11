 BEGIN_PROVIDER [ double precision, ao_nucl_elec_integral_pseudo, (ao_num_align,ao_num)]
 BEGIN_DOC
! interaction nuclear electron
 END_DOC
 implicit none
 double precision  :: alpha, beta, gama, delta
 integer           :: num_A,num_B
 double precision  :: A_center(3),B_center(3),C_center(3)
 integer           :: power_A(3),power_B(3)
 integer           :: i,j,k,l,n_pt_in,m
 double precision  ::  Vloc, Vpseudo

 double precision  :: cpu_1, cpu_2, wall_1, wall_2, wall_0
 integer           :: thread_num

  ao_nucl_elec_integral_pseudo = 0.d0

  !                 
  ! |   _   _  _. | 
  ! |_ (_) (_ (_| | 
  !                 
  !! Parameters of the local part of pseudo:

!  integer klocmax
!  integer, allocatable ::  n_k(:,:)
!  double precision, allocatable ::  v_k(:,:), dz_k(:,:)
!
!  call ezfio_get_pseudo_klocmax(klocmax)
!
!  allocate(n_k(nucl_num,klocmax),v_k(nucl_num,klocmax), dz_k(nucl_num,klocmax))
!
!  call ezfio_get_pseudo_v_k(v_k)
!  call ezfio_get_pseudo_n_k(n_k)
!  call ezfio_get_pseudo_dz_k(dz_k)
!
  !! Dump array 
  integer, allocatable ::  n_k_dump(:)
  double precision, allocatable ::  v_k_dump(:), dz_k_dump(:) 
 
  allocate(n_k_dump(1:pseudo_klocmax), v_k_dump(1:pseudo_klocmax), dz_k_dump(1:pseudo_klocmax)) 


  !                               
  ! |\ |  _  ._    |  _   _  _. | 
  ! | \| (_) | |   | (_) (_ (_| | 
  !                              
  !! Parameters of non local part of pseudo:

!  integer :: kmax,lmax
!  integer, allocatable ::  n_kl(:,:,:)
!  double precision, allocatable ::  v_kl(:,:,:), dz_kl(:,:,:) 
!
!  call ezfio_get_pseudo_lmaxpo(lmax)
!  call ezfio_get_pseudo_kmax(kmax)
!  !lmax plus one -> lmax
!  lmax = lmax - 1 
! 
!  allocate(n_kl(nucl_num,kmax,0:lmax), v_kl(nucl_num,kmax,0:lmax), dz_kl(nucl_num,kmax,0:lmax)) 
!
!  call ezfio_get_pseudo_n_kl(n_kl)
!  call ezfio_get_pseudo_v_kl(v_kl)
!  call ezfio_get_pseudo_dz_kl(dz_kl)
!
!
  !! Dump array 
  integer, allocatable ::  n_kl_dump(:,:)
  double precision, allocatable ::  v_kl_dump(:,:), dz_kl_dump(:,:) 
 
  allocate(n_kl_dump(pseudo_kmax,0:pseudo_lmax), v_kl_dump(pseudo_kmax,0:pseudo_lmax), dz_kl_dump(pseudo_kmax,0:pseudo_lmax)) 

  !  _                
  ! /   _. |  _     | 
  ! \_ (_| | (_ |_| | 
  !                   

  write(output_monoints,*) 'Providing the nuclear electron pseudo integrals '

  call wall_time(wall_1)
  call cpu_time(cpu_1)

  !$OMP PARALLEL &
  !$OMP DEFAULT (NONE) &
  !$OMP PRIVATE (i,j,k,l,m,alpha,beta,A_center,B_center,C_center,power_A,power_B, &
  !$OMP          num_A,num_B,Z,c,n_pt_in, &
  !$OMP          v_k_dump,n_k_dump, dz_k_dump, n_kl_dump, v_kl_dump, dz_kl_dump, &
  !$OMP          wall_0,wall_2,thread_num, output_monoints) & 
  !$OMP SHARED (ao_num,ao_prim_num,ao_expo_ordered_transp,ao_power,ao_nucl,nucl_coord,ao_coef_normalized_ordered_transp, &
  !$OMP         ao_nucl_elec_integral_pseudo,nucl_num,nucl_charge, &
  !$OMP         pseudo_klocmax,pseudo_lmax,pseudo_kmax,pseudo_v_k,pseudo_n_k, pseudo_dz_k, pseudo_n_kl, pseudo_v_kl, pseudo_dz_kl, &
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
  

        n_kl_dump = pseudo_n_kl(k,1:pseudo_kmax,0:pseudo_lmax)
        v_kl_dump = pseudo_v_kl(k,1:pseudo_kmax,0:pseudo_lmax)
        dz_kl_dump = pseudo_dz_kl(k,1:pseudo_kmax,0:pseudo_lmax)

        c = c + Vpseudo(pseudo_lmax,pseudo_kmax,v_kl_dump,n_kl_dump,dz_kl_dump,A_center,power_A,alpha,B_center,power_B,beta,C_center)
  
      enddo
      ao_nucl_elec_integral_pseudo(i,j) = ao_nucl_elec_integral_pseudo(i,j) + &
                                   ao_coef_normalized_ordered_transp(l,j)*ao_coef_normalized_ordered_transp(m,i)*c
     enddo
     enddo
  enddo

    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        write(output_monoints,*) 100.*float(j)/float(ao_num), '% in ',  &
                                 wall_2-wall_1, 's'
      endif
    endif
  enddo

 !$OMP END DO
 !$OMP END PARALLEL


!  _                                 
! | \  _   _. | |  _   _  _. _|_  _  
! |_/ (/_ (_| | | (_) (_ (_|  |_ (/_ 
!                                    
 
!  deallocate(n_k,v_k, dz_k)
  deallocate(n_k_dump,v_k_dump, dz_k_dump)

!  deallocate(n_kl,v_kl, dz_kl)
  deallocate(n_kl_dump,v_kl_dump, dz_kl_dump)


END_PROVIDER
