Module geometry

  Use parameters

  Implicit None

Contains

    ! Ajoute un point
    Subroutine add_vertice(vertices_list,vertices_bool,end_vertices,x,y)

        Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices_list
        Logical,Dimension(array_size),Intent(Inout)    :: vertices_bool
        Integer,Intent(Inout)                          :: end_vertices
        Real(Pr),Intent(In)                            :: x,y

        end_vertices = end_vertices+1
        vertices_bool(end_vertices) = .true.
        vertices_list(2*end_vertices-1) = x
        vertices_list(2*end_vertices) = y
        nb_vertices = nb_vertices+1

    End Subroutine add_vertice

    ! Ajoute un triangle pour 3 points donnees
    Subroutine add_triangle(triangles_list,triangles_bool,end_triangles,s1,s2,s3)

        Integer,Dimension(3*array_size),Intent(Inout) :: triangles_list
        Logical,Dimension(array_size),Intent(Inout)   :: triangles_bool
        Integer,Intent(Inout)                         :: end_triangles
        Integer,Intent(In)                            :: s1,s2,s3

        end_triangles = end_triangles+1
        triangles_bool(end_triangles) = .true.
        triangles_list(3*end_triangles-2) = s1
        triangles_list(3*end_triangles-1) = s2
        triangles_list(3*end_triangles) = s3
        nb_triangles = nb_triangles+1

    End Subroutine add_triangle

    ! Renvoie l'indice de l'arete, -1 s'il ne la trouve pas
    ! Cherche dans la liste des aretes de bord
    Function is_border(edge1,edge2,edges_list,end_edges,edges_bool) Result(ind)

        Integer,Dimension(2*array_size),Intent(In) :: edges_list
        Integer,Intent(In)                         :: edge1,edge2,end_edges
        Logical,Dimension(array_size),Intent(In)   :: edges_bool
        Integer                                    :: i,s1,s2,ind
        Logical                                    :: b1,b2

        ind = -1

        Do i=1,end_edges
            If(edges_bool(i))Then
                s1 = edges_list(2*i-1)
                s2 = edges_list(2*i)
                b1 = (edge1 .EQ. s1) .OR. (edge1 .EQ. s2)
                b2 = (edge2 .EQ. s1) .OR. (edge2 .EQ. s2)
                If(b1 .AND. b2)Then
                    ind = i
                End IF
            End If
        End Do

    End Function is_border

    ! Renvoie les sommets du milieu d'une arete donnee
    Subroutine middle_edge(edge1,edge2,vertices_list,x,y)

        Integer,Intent(In)                          :: edge1,edge2
        Real(Pr),Dimension(2*array_size),Intent(In) :: vertices_list
        Real(Pr),Intent(Inout)                      :: x,y
        Real(Pr)                                    :: x1,y1,x2,y2

        x1 = vertices_list(2*edge1-1)
        y1 = vertices_list(2*edge1)
        x2 = vertices_list(2*edge2-1)
        y2 = vertices_list(2*edge2)

        x = (x1+x2)/2._Pr
        y = (y1+y2)/2._Pr

    End Subroutine middle_edge

    ! Renvoie true si le sommet appartient au triangle
    Function vertice_triangle(s1,tri1,tri2,tri3) Result(b)

        Integer,Intent(In) :: s1,tri1,tri2,tri3
        Logical            :: b

        b = (s1 .EQ. tri1) .OR. (s1 .EQ. tri2) .OR. (s1 .EQ. tri3)

    End Function vertice_triangle

    ! Renvoie true si l'arete appartient au triangle
    Function edge_triangle(edge1,edge2,tri1,tri2,tri3) Result(b)

        Integer,Intent(In) :: edge1,edge2,tri1,tri2,tri3
        Logical            :: b,b1,b2

        b1 = (edge1 .EQ. tri1) .OR. (edge1 .EQ. tri2) .OR. (edge1 .EQ. tri3)
        b2 = (edge2 .EQ. tri1) .OR. (edge2 .EQ. tri2) .OR. (edge2 .EQ. tri3)

        b = (b1 .AND. b2)

    End Function edge_triangle

    ! Renvoie le 3eme sommet pour un triangle et une arete donnees
    Function find_summit(edge1,edge2,triangles_list,ind) Result(summit)

        Integer,Intent(In)                         :: edge1,edge2,ind
        Integer,Dimension(3*array_size),Intent(In) :: triangles_list
        Integer                                    :: summit,tri1,tri2,tri3

        tri1 = triangles_list(3*ind-2)
        tri2 = triangles_list(3*ind-1)
        tri3 = triangles_list(3*ind)

        If((edge1 .NE. tri1) .AND. (edge2 .NE. tri1))Then
            summit = tri1
        ElseIf((edge1 .NE. tri2) .AND. (edge2 .NE. tri2))Then
            summit = tri2
        Else
            summit = tri3
        End If

    End Function find_summit

    ! Trouve le triangle contenant l'arete donnee
    ! Renvoie l'indice du triangle, -1 si il ne trouve pas
    Function find_triangle(edge1,edge2,triangles_list,triangles_bool,end_triangles) &
                           &Result(ind_triangle)

        Integer,Intent(In)                         :: edge1,edge2,end_triangles
        Integer,Dimension(3*array_size),Intent(In) :: triangles_list
        Logical,Dimension(array_size),Intent(In)   :: triangles_bool
        Integer                                    :: ind_triangle,i

        ind_triangle = -1

        Do i=1,end_triangles
            If(triangles_bool(i))Then
                If(edge_triangle(edge1,edge2,triangles_list(3*i-2),triangles_list(3*i-1),&
                   &triangles_list(3*i)))Then
                    ind_triangle = i
                End If
            End If
        End Do

    End Function find_triangle

  ! Renvoie une liste contenant la longueur des aretes de bord
  Function length_list(edges, end_edges, vertices)

    Real(Pr),Dimension(:),Intent(In) :: vertices
    Integer,Dimension(:),Intent(In)  :: edges
    Integer,Intent(In)               :: end_edges
    Real(Pr),Dimension(end_edges)    :: length_list
    Real(Pr)                         :: length, x1, x2, y1, y2
    Integer                          :: ind,i

    Do i=1,end_edges
       ind = edges(2*i-1)
       x1 = vertices(2*ind-1)
       y1 = vertices(2*ind)
       ind = edges(2*i)
       x2 = vertices(2*ind-1)
       y2 = vertices(2*ind)
       length = edge_length(x1,y1,x2,y2)
       length_list(i) = length
    End Do

  End Function length_list

  ! Renvoie la longueur d'une arete
  Function edge_length(x1,y1,x2,y2) Result(length)

    Real(Pr),Intent(In) :: x1,y1,x2,y2
    Real(Pr)            :: length

    length = sqrt((x2-x1)**2 + (y2-y1)**2)

  End Function edge_length

  ! Calcule l'aire d'un triangle en 2D
  Function triangle_area(x1,y1,x2,y2,x3,y3) Result(Area)

    Real(Pr),Intent(In) :: x1,x2,x3,y1,y2,y3
    Real(Pr)            :: Area

    Area = abs(0.5*(det(x2-x1, y2-y1, x3-x1, y3-y1)))

  End Function triangle_area

  ! Calcule la longueur des aretes au carre d'un element
  Function edge_length_elem(vertices,dimen) Result(length)

    Integer,Intent(In)          :: dimen
    Real(Pr),Dimension(2*dimen) :: vertices
    Real(Pr)                    :: length
    Integer                     :: i

    length = 0._Pr

    Do i = 1,dimen
      length = length + (vertices(dimen+i)-vertices(i))**2
    End Do

  End Function edge_length_elem

  ! Calcule un détermiant 2D de la forme
  ! | a c |
  ! | b d |
  Function det(a,b,c,d)

    Real(Pr),Intent(In) :: a,b,c,d
    Real(Pr)            :: det

    det = a*d-b*c

  End Function det

  ! Renvoie le rayon du cercle circonscirt pour un triangle donné
  Function circle_ray(l1,l2,l3,area) Result(ray)

    Real(Pr),Intent(In) :: l1,l2,l3,area
    Real(Pr)            :: ray

    ray = l1*l2*l3/(4._Pr*area)

  End Function circle_ray

  ! Renvoie le centre du cercle circonscirt pour un triangle donné
  Function circle_centre(x1,y1,x2,y2,x3,y3) Result(center)

      Real(Pr),Intent(In)   :: x1,x2,x3,y1,y2,y3
      Real(Pr),Dimension(2) :: center
      Real(Pr)              :: delta,l1,l2,l3

      delta = 2._Pr*(det(x2,x3,y2,y3)-det(x1,x3,y1,y3)+det(x1,x2,y1,y2))

      l1 = x1**2._Pr + y1**2._Pr
      l2 = x2**2._Pr + y2**2._Pr
      l3 = x3**2._Pr + y3**2._Pr

      center(1) = det(l2,l3,y2,y3)-det(l1,l3,y1,y3)+det(l1,l2,y1,y2)
      center(1) = (1._Pr/delta)*center(1)
      center(2) = det(l2,l3,x2,x3)-det(l1,l3,x1,x3)+det(l1,l2,x1,x2)
      center(2) = -(1._Pr/delta)*center(2)

  End Function circle_centre

End Module geometry
