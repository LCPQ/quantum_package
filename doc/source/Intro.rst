============================================================
What is a selected CI caculation ? Some theoretical concepts
============================================================
  
Generalities
============

 The selected CI algorithm can be seen as a way to compute the energies (and various properties) of a given number of eigenstates 
 of a given :term:`target space` (ex : CISD, CAS-CI, DDCI etc ...), 
 but by taking the freedom of splitting the wave function of the target space in term
 of :term:`internal determinants` treated variationally and :term:`perturbers` treated perturbatively. 
 Why this freedom ? Because in a given :term:`target space` (except some really special cases) most of the information
 is concentrated within a tiny fraction of the :term:`target wave function`, and the remaining part can be reasonabely estimated by perturbtation.


 This splitting of the wave function is not done in one shot, it is done iteratively. The iterative procedure needs a :term:`stopping criterion` to end the calculation and to control the quality of the calculation.
 This :term:`stopping criterion` can be for example the number of determinants in the :term:`intern space`,
 or the value of the :term:`energetic perturbative correction` to estimate the importance of the perturbation, or the convergence of the :term:`estimated target energy`, or anything that can 
 be defined in terms of available informations during the calculation.
 
 The heart of the selected CI algorithm is based on the CIPSI algorithm (ref Malrieu). 

 
Selected CI in a few words
==========================

 First you define a :term:`target space`. Once the target space is defined, you define the :term:`stopping criterion`.
 After that, a starting wave function is chosen by the user (HF by default). 
 This starting wave function is the first :term:`Internal determinants` wave function. 
 After that, one would like to extend this :term:`Internal determinants` wave function by adding 
 some :term:`perturbers` determinants.

 How do we select the good :term:`perturbers` ?
 

  do while (:term:`stopping criterion` is reached)
  
  
  1) Generates :term:`perturbers` determinants according to your chosen :term:`target space`.

     :math:`\Rightarrow` generates a set of :term:`perturbers` :math:`\{|D_P\rangle\}`
  2) The :term:`perturbers` importance are estimated by perturbation thanks to the current :term:`internal determinants`.
  3) The most important of the :math:`\{|D_P\rangle\}` are chosen to enter in the :term:`internal determinants`.
  4) You rediagonalize the H matrix with the previous set of :term:`internal determinants` and the chosen :term:`perturbers`.

     :math:`\Rightarrow` create a new wave function and a new set of the :term:`internal determinants`
  5) iterate

Once the iterative procedure is stopped, the :term:`internal determinants` wave function have a :term:`variational energy`, 
and by adding the :term:`energetic perturbative correction` one have the :term:`estimated target energy`  
which is an approximation of the :term:`target energy`. One should notice that if one takes 
a :term:`stopping criterion` such as the all the determinants of the target space are in the :term:`intern space`, 
the :term:`estimated target energy` is the :term:`target energy`.


If one is interested of how is built the selected CI wave function into more details, one can read the further section.

What is a selected CI iteration in practice (and some details)
==============================================================

 From the previous section we have roughly seen how the selected CI works. Now, getting a bit more into details, 
 we will see what is done in practice during a selected CI iteration. To illustrate this, a simple CISD example wil be given.

The general picture
^^^^^^^^^^^^^^^^^^^  

The :term:`target space` defines entirely the method that is going to be approximated, and the stopping criterion will be the only approximation. 
This :term:`target space` can always be defined in terms of application of an :term:`H operator` 
(with some :term:`excitation restrictions`) on a given set of determinants that we shall call the :term:`generators` determinants.
We call :term:`restricted H operator` this precise H operator.
The target space intirely defines the :term:`restricted H operator`.
The only flexibility is the perturbation theory to be used to estimate the coeficients of the :term:`perturbers`. 
If the target space is just defines in term of a CI matrix to diagonalize, the standard :term:`Diagonalization EN EG` perturbation will be used.
If some other constraints are imposed in addition to the CI matrix 
(e.g. some physical conditions of size extensivity such as in the :term:`CISD+SC2` method), 
then the perturbation must be adapted to properly respect the :term:`target space`.

CISD : the :term:`target space` is here defined intirely by all the single and double excitations acting on the HF determinant.
So the :term:`generators` subset of determinants here is only the HF determinant and will not change along the iteration.
If some occupied orbitals are chosen to be frozen (no excitations from those orbitals) 
or some virtuals are chosen to be deleted (no excitations going to these virtuals orbitals), 
this constraint imposes the :term:`excitation restrictions`. So here the :term:`restricted H operator` will be all the single and double excitations except those involving either a frozen core orbital or a deleted virtual orbital.
Different choices of perturbation theory can be made for the CISD, but the standard :term:`Diagonalization EN EG` can be trustly used.

Once the :term:`target space` have been defined, what does in practice a selected CI iteration.
For the sake of simplicity, here we emphasize on the ground state :math:`| \psi_0 \rangle`. At a given iteration, one have a :term:`selected wave function` :math:`|\psi_0\rangle`, and the selected CI algorithm performs : 

 do G = 1, N_Generators
  
  1) Apply the :term:`restricted H operator` on the :math:`|D_G \rangle` :term:`generators` determinant belonging to :math:`| \psi_0 \rangle`

     :math:`\Rightarrow` generates a set of :term:`perturbers` :math:`|D_P\rangle`

  2) Estimate the perturbative importance of each perturbers 
     
     :math:`\Rightarrow` example for the :term:`EN EG` perturbation theory

     .. math::

       c_{D_P}^0=  \frac{ \sum_{S=1,N_{\rm selectors}} c_S^0 \langle D_S|H|D_P\rangle}{  \langle \psi_0 |H|\psi_0 \rangle - \langle D_P |H|D_P\rangle } \\
       e_{D_P}^0=  \frac{(\sum_{S=1,N_{\rm selectors}}  c_S^0 \langle D_S|H|D_P\rangle) ^2}{\langle \psi_0 |H|\psi_0 \rangle - \langle D_P |H|D_P\rangle}

  3) Keep the most important :term:`perturbers` :math:`|D_P \rangle`

     :math:`\Rightarrow` they enter in the :term:`intern space`
     
  4) Rediagonalize H within this new subset of determinants 

     :math:`\Rightarrow` better :term:`selected wave function`

  5) Iterate

An important point here is that at a given iteration, the estimation of the perturbative coefficients of the :term:`perturbers` 
depends on the quality of the :term:`selected wave function` . 
As the iterations go on, the :term:`selected wave function` becomes closer 
and closer to the :term:`target wave function`, and so the perturbative estimation of the :term:`perturbers` coefficients or energetic contribution becomes more and more precise.


CISD : At the first iteration, starting from the HF determinant : 
    1) By applying H on the :term:`generators` (HF) one generates all singles and doubles 
    2) For each :term:`perturbers` you estimate by perturbation its coefficient of energetic contribution. 

       i) Here the :term:`selectors` is only the HF determinant.

       ii) If the :term:`Brillouin theorem` is respected, all the singles have zero coefficients since the :term:`selectors` here is only the HF determinant.  
       iii) The most important double excitations entered
       iv) The :term:`energetic perturbative correction` is calculated
       v) The :term:`estimated target energy` is just the sum of the HF energy and the :term:`energetic perturbative correction`

    3) H is rediagonlaized in the new set of determinants : HF + the selected doubles
    
      :math:`\Rightarrow` better :term:`variational energy` and :term:`selected wave function`

    4) The :term:`generators` subset does not change.


      At the second iteration :
    1) By applying H on the :term:`generators` (still HF) one generates all singles and doubles 
    2) For each :term:`perturbers` you estimate by perturbation its coefficient of energetic contribution. 

       i) Here the :term:`selectors` is now HF + the previously selected doubles
    
      :math:`\Rightarrow` the :term:`perturbers` now interact with all the previously selected doubles 
      :math:`\Rightarrow` better estimation of the coefficients of the :term:`perturbers`
      :math:`\Rightarrow` the singles have non zero coefficients

       ii) The most important :term:`perturbers` enter in the :term:`intern space`

       iv) The :term:`energetic perturbative correction` is re estimated 

       v) The :term:`estimated target energy` is now the sum of the variational energy of the :term:`selected wave function` and the :term:`energetic perturbative correction`
    
      :math:`\Rightarrow` better estimation of the :term:`target energy`

      Iterate untill you reached the desired :term:`stopping criterion`

