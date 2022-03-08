Module delaunay

  Use parameters
  Use geometry

  Implicit None

Contains

  ! Noyau de delaunay
  Subroutine delaunay_core(triangles_list,triangles_bool,vertices_list,vertices_bool,end_triangles,end_vertices,p_ind)

      Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices_list
      Integer,Dimension(3*array_size),Intent(Inout)  :: triangles_list
      Logical,Dimension(array_size),Intent(Inout)    :: triangles_bool,vertices_bool
      Integer,Intent(Inout)                          :: end_triangles,end_vertices
      Integer,Intent(In)                             :: p_ind
      Integer,Dimension(3)                           :: ind_list
      Integer                                        :: i, j, ind1, ind2, ind3, in_ind, next_ind
      Integer                                        :: end_cavity
      Real(Pr)                                       :: x1,x2,x3,y1,y2,y3,p1,p2
      Integer,Dimension(2*cavity_size)               :: cavity_edges
      Logical,Dimension(cavity_size)                 :: cavity_bool

      ! Initialisation de certaines variables
      end_cavity = 0
      cavity_bool = .false.
      p1 = vertices_list(2*p_ind-1)
      p2 = vertices_list(2*p_ind)

      Do i=1,end_triangles
          If(triangles_bool(i))Then

              ind1 = triangles_list(3*i-2)
              ind2 = triangles_list(3*i-1)
              ind3 = triangles_list(3*i)

              x1 = vertices_list(2*ind1-1)
              y1 = vertices_list(2*ind1)
              x2 = vertices_list(2*ind2-1)
              y2 = vertices_list(2*ind2)
              x3 = vertices_list(2*ind3-1)
              y3 = vertices_list(2*ind3)

              If(is_inside(x1,y1,x2,y2,x3,y3,p1,p2))Then
                  ! Supprime les triangles
                  triangles_bool(i) = .false.
                  nb_triangles = nb_triangles - 1

                  ! Ajoute les aretes du triangle supprime
                  ind_list(1) = ind1
                  ind_list(2) = ind2
                  ind_list(3) = ind3
                  Call sort_list(ind_list, 3)

                  end_cavity = end_cavity+1
                  cavity_bool(end_cavity) = .true.
                  cavity_edges(2*end_cavity-1)=ind_list(1)
                  cavity_edges(2*end_cavity)=ind_list(2)
                  end_cavity = end_cavity+1
                  cavity_bool(end_cavity) = .true.
                  cavity_edges(2*end_cavity-1)=ind_list(1)
                  cavity_edges(2*end_cavity)=ind_list(3)
                  end_cavity = end_cavity+1
                  cavity_bool(end_cavity) = .true.
                  cavity_edges(2*end_cavity-1)=ind_list(2)
                  cavity_edges(2*end_cavity)=ind_list(3)
              End If
          End If
      End Do

      Call find_duplication(cavity_edges, cavity_bool, end_cavity)

      Do i=1,end_cavity
          If(cavity_bool(i))Then
              Call add_triangle(triangles_list,triangles_bool,end_triangles,&
                                &cavity_edges(2*i-1),cavity_edges(2*i),p_ind)
          End If
      End Do

  End Subroutine delaunay_core

  ! Trie une liste d'entier
  Subroutine sort_list(list, end_list)

      Integer,Dimension(:),Intent(Inout) :: list
      Integer,Intent(In)                 :: end_list
      Integer                            :: i,j,xmax,ind_max,xj,ind_j,ind_prov

      Do i=1,end_list-1
         xmax = list(1)
         ind_max = 1
         Do j=2,end_list-i+1
             xj = list(j)
             ind_j = j
             If(xj .GT. xmax)Then
                 xmax = xj
                 ind_max = ind_j
             End If
         End do
         ind_prov = list(end_list-i+1)
         list(end_list-i+1) = list(ind_max)
         list(ind_max) = ind_prov
     End Do

  End Subroutine sort_list

  ! Repere les aretes en double dans la liste des aretes de la cavite
  Subroutine find_duplication(edges_list, edges_bool, ind_end)

      Integer,Dimension(2*cavity_size),Intent(In)  :: edges_list
      Logical,Dimension(cavity_size),Intent(Inout) :: edges_bool
      Integer,Intent(In)                           :: ind_end
      Integer                                      :: i,j,s1,s2

      Do i=1,ind_end-1
          s1 = edges_list(2*i-1)
          s2 = edges_list(2*i)
          Do j = i+1, ind_end
              If(edges_bool(j))Then
                  If((edges_list(2*j-1) .EQ. s1) .AND. (edges_list(2*j) .EQ. s2))Then
                      edges_bool(j) = .false.
                      edges_bool(i) = .false.
                  End If
              End If
          End Do
      End Do

  End Subroutine find_duplication

  ! Determine si un point est dans le cercle circonscrit d'un triangle donne
  Function is_inside(x1,y1,x2,y2,x3,y3,p1,p2) Result(bool)

    Real(Pr),Intent(In)   ::  x1,x2,x3,y1,y2,y3,p1,p2
    Logical               :: bool
    Real(Pr),Dimension(2) :: center
    Real(Pr)              :: ray,l1,l2,l3,area,l

    center = circle_centre(x1,y1,x2,y2,x3,y3)
    l1 = edge_length(x1,y1,x2,y2)
    l2 = edge_length(x2,y2,x3,y3)
    l3 = edge_length(x1,y1,x3,y3)
    area = triangle_area(x1,y1,x2,y2,x3,y3)
    ray = circle_ray(l1,l2,l3,area)

    l = dsqrt((center(1)-p1)**2._Pr + (center(2)-p2)**2._Pr)

    If(l .le. ray)Then
       bool = .true.
    Else
       bool = .false.
    End If

  End Function is_inside

  ! Determine les sommets de la boite englobante
  Subroutine box(vertices, vertices_bool, end_vertices, end_edges, edges_list)

    Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices
    Logical,Dimension(array_size),Intent(Inout)    :: vertices_bool
    Integer,Dimension(2*array_size),Intent(In)     :: edges_list
    Integer,Intent(Inout)                          :: end_vertices
    Integer,Intent(In)                             :: end_edges
    Integer                                        :: i
    Real(Pr)                                       :: min_ab,max_ab,min_or,max_or,ab,or,mean_length
    Real(Pr)                                       :: factor

    mean_length = sum(length_list(edges_list, end_edges, vertices))/end_edges

    min_ab = vertices(1)
    max_ab = vertices(1)
    min_or = vertices(2)
    max_or = vertices(2)

    ! Determine le min et le max des abscisses et des ordonnees
    Do i=2,end_vertices
       If(vertices_bool(i))Then
         ab = vertices(2*i-1)
         or = vertices(2*i)
         If(ab .LT. min_ab)Then
           min_ab = ab
         End If
         If(ab .GT. max_ab)Then
           max_ab = ab
         End If
         If(or .LT. min_or)Then
            min_or = or
          End If
          If(or .GT. max_or)Then
            max_or = or
         End If
       End If
    End Do

    Do i=1,4
      vertices_bool(i+end_vertices) = .true.
    End Do

    factor = 1._Pr

    min_ab = min_ab - factor*mean_length
    max_ab = max_ab + factor*mean_length
    min_or = min_or - factor*mean_length
    max_or = max_or + factor*mean_length

    ! Ajout des points de la boite englobante dans la liste des sommets
    vertices(2*(1+end_vertices)-1) = min_ab
    vertices(2*(1+end_vertices)) = min_or
    vertices(2*(2+end_vertices)-1) = max_ab
    vertices(2*(2+end_vertices)) = min_or
    vertices(2*(3+end_vertices)-1) = max_ab
    vertices(2*(3+end_vertices)) = max_or
    vertices(2*(4+end_vertices)-1) = min_ab
    vertices(2*(4+end_vertices)) = max_or

    end_vertices = end_vertices + 4
    nb_vertices = nb_vertices + 4

  End Subroutine box

End Module delaunay
