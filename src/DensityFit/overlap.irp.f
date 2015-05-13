double precision function ao_four_overlap(i,j,k,l)
  implicit none
  BEGIN_DOC
! \int \chi_i(r) \chi_j(r) \chi_k(r) \chi_l(r) dr
  END_DOC
  integer,intent(in)             :: i,j,k,l
  integer                        :: p,q,r,s
  double precision               :: I_center(3),J_center(3),K_center(3),L_center(3)
  integer                        :: num_i,num_j,num_k,num_l,dim1,I_power(3),J_power(3),K_power(3),L_power(3)
  double precision               :: overlap_x,overlap_y,overlap_z, overlap
  include 'include/constants.F'
  double precision               :: P_new(0:max_dim,3),P_center(3),fact_p,pp
  double precision               :: Q_new(0:max_dim,3),Q_center(3),fact_q,qq
  integer                        :: iorder_p(3), iorder_q(3)
  
  dim1 = n_pt_max_integrals
  
  num_i = ao_nucl(i)
  num_j = ao_nucl(j)
  num_k = ao_nucl(k)
  num_l = ao_nucl(l)
  ao_four_overlap = 0.d0
  
  do p = 1, 3
    I_power(p) = ao_power(i,p)
    J_power(p) = ao_power(j,p)
    K_power(p) = ao_power(k,p)
    L_power(p) = ao_power(l,p)
    I_center(p) = nucl_coord(num_i,p)
    J_center(p) = nucl_coord(num_j,p)
    K_center(p) = nucl_coord(num_k,p)
    L_center(p) = nucl_coord(num_l,p)
  enddo
  
  do p = 1, ao_prim_num(i)
    double precision               :: coef1
    coef1 = ao_coef_normalized_ordered_transp(p,i)
    do q = 1, ao_prim_num(j)
      call give_explicit_poly_and_gaussian(P_new,P_center,pp,fact_p,iorder_p,&
          ao_expo_ordered_transp(p,i),ao_expo_ordered_transp(q,j),                 &
          I_power,J_power,I_center,J_center,dim1)
      double precision               :: coef2
      coef2 = coef1*ao_coef_normalized_ordered_transp(q,j)*fact_p
      do r = 1, ao_prim_num(k)
        double precision               :: coef3
        coef3 = coef2*ao_coef_normalized_ordered_transp(r,k)
        do s = 1, ao_prim_num(l)
          double precision               :: general_primitive_integral
          call give_explicit_poly_and_gaussian(Q_new,Q_center,qq,fact_q,iorder_q,&
              ao_expo_ordered_transp(r,k),ao_expo_ordered_transp(s,l),             &
              K_power,L_power,K_center,L_center,dim1)
          double precision               :: coef4
          coef4 = coef3*ao_coef_normalized_ordered_transp(s,l)*fact_q
          call overlap_gaussian_xyz(P_center,Q_center,pp,qq,iorder_p,iorder_q,overlap_x,overlap_y,overlap_z,overlap,dim1)
          ao_four_overlap +=  coef4 * overlap 
        enddo ! s
      enddo  ! r
    enddo   ! q
  enddo    ! p
    
end

! TODO : Schwartz acceleration



