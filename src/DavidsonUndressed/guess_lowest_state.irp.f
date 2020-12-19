program first_guess
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Select all the determinants with the lowest energy as a starting point.
  END_DOC
  integer                        :: i,j
  double precision, allocatable  :: orb_energy(:)
  double precision               :: E
  integer, allocatable           :: kept(:)
  integer                        :: nelec_kept(2)
  character                      :: occ_char, keep_char
  
  PROVIDE H_apply_buffer_allocated psi_det
  allocate (orb_energy(mo_tot_num), kept(0:mo_tot_num))
  nelec_kept(1:2) = 0
  kept(0) = 0

  print *,  'Orbital energies'
  print *,  '================'
  print *,  ''
  do i=1,mo_tot_num
    keep_char = ' '
    occ_char = '-'
    orb_energy(i) = mo_mono_elec_integral(i,i)
    do j=1,elec_beta_num
      if (i==j) cycle
      orb_energy(i) += mo_bielec_integral_jj_anti(i,j)
    enddo
    do j=1,elec_alpha_num
      orb_energy(i) += mo_bielec_integral_jj(i,j)
    enddo
    if ( (orb_energy(i) > -.5d0).and.(orb_energy(i) < .1d0) ) then
      kept(0) += 1
      keep_char = 'X'
      kept( kept(0) ) = i
      if (i <= elec_beta_num) then
        nelec_kept(2) += 1
      endif
      if (i <= elec_alpha_num) then
        nelec_kept(1) += 1
      endif
    endif
    if (i <= elec_alpha_num) then
      if (i <= elec_beta_num) then
        occ_char = '#'
      else
        occ_char = '+'
      endif
    endif
    print '(I4, 3X, A, 3X, F10.6, 3X, A)', i, occ_char, orb_energy(i), keep_char 
  enddo

  
  integer, allocatable           :: list  (:,:)
  integer(bit_kind), allocatable :: string(:,:)
  allocate ( list(N_int*bit_kind_size,2), string(N_int,2) )

  string = ref_bitmask
  call bitstring_to_list( string(1,1), list(1,1), elec_alpha_num, N_int)
  call bitstring_to_list( string(1,2), list(1,2), elec_beta_num , N_int)

  psi_det_alpha_unique(:,1) = string(:,1)
  psi_det_beta_unique (:,1) = string(:,2)
  N_det_alpha_unique = 1
  N_det_beta_unique  = 1

  integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

  psi_det_size = kept(0)**(nelec_kept(1)+nelec_kept(2))
  print *,  kept(0), nelec_kept(:)
  call write_int(6,psi_det_size,'psi_det_size')
  TOUCH psi_det_size

BEGIN_SHELL [ /usr/bin/env python2 ]

template_alpha_ext = """
do %(i2)s = %(i1)s-1,1,-1
  list(elec_alpha_num-%(i)d,1) = kept(%(i2)s)
  call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
"""

template_alpha = """
do %(i2)s = %(i1)s-1,1,-1
  list(elec_alpha_num-%(i)d,1) = kept(%(i2)s)
  call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
  N_det_alpha_unique += 1
  psi_det_alpha_unique(:,N_det_alpha_unique) = string(:,1)
"""

template_beta_ext = """
do %(i2)s = %(i1)s-1,1,-1
  list(elec_beta_num-%(i)d,2) = kept(%(i2)s)
  call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
"""
template_beta = """
do %(i2)s = %(i1)s-1,1,-1
  list(elec_beta_num-%(i)d,2) = kept(%(i2)s)
  call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
  N_det_beta_unique += 1
  psi_det_beta_unique(:,N_det_beta_unique) = string(:,2)
"""

def write(template_ext,template,imax):
    print "case(%d)"%(imax)
    def aux(i2,i1,i,j):
        if (i==imax-1):
          print template%locals()
        else:
          print template_ext%locals()
        i += 1
        j -= 1
        if (i != imax):
            i1 = "i%d"%(i)
            i2 = "i%d"%(i+1)
            aux(i2,i1,i,j)
        print "enddo"

    i2 = "i1"
    i1 = "kept(0)+1"
    i  = 0
    aux(i2,i1,i,imax)

def main():
    print """
    select case (nelec_kept(1))
      case(0)
        continue
    """
    for imax in range(1,10):
        write(template_alpha_ext,template_alpha,imax)

    print """
    end select

    select case (nelec_kept(2))
      case(0)
        continue
    """
    for imax in range(1,10):
        write(template_beta_ext,template_beta,imax)
    print "end select"

main()

END_SHELL

  TOUCH N_det_alpha_unique N_det_beta_unique psi_det_alpha_unique psi_det_beta_unique
  call create_wf_of_psi_bilinear_matrix(.False.)
  call diagonalize_ci
  j= N_det
  do i=1,N_det
    if (psi_average_norm_contrib_sorted(i) < 1.d-6) then
       j = i-1
       exit
    endif
!  call debug_det(psi_det_sorted(1,1,i),N_int)
  enddo
  call save_wavefunction_general(j,N_states,psi_det_sorted,size(psi_coef_sorted,1),psi_coef_sorted)

  deallocate(orb_energy, kept, list, string)
end
