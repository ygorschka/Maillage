Module border

    Use parameters
    Use geometry

    Implicit None

Contains

    ! Supprime les triangles exterieurs
    Subroutine delete_out_of_border(triangles_list,triangles_bool,end_triangles,edges_list,&
                                    &edges_end,vertices_bool,end_vertices,edges_bool)

        Integer,Dimension(3*array_size),Intent(Inout) :: triangles_list
        Logical,Dimension(array_size),Intent(Inout)   :: triangles_bool,vertices_bool,edges_bool
        Integer,Intent(Inout)                         :: end_triangles
        Integer,Intent(In)                            :: edges_end
        Integer,Intent(Inout)                         :: end_vertices
        Integer,Dimension(2*array_size),Intent(In)    :: edges_list
        !Integer,Dimension(3*array_size)               :: triangles_list2
        Logical,Dimension(array_size)                 :: triangles_bool2
        Integer                                       :: i, nb_triangles2

        ! Appel initial
        Call delete_triangle(triangles_list,triangles_bool,end_triangles,edges_list,&
                             &edges_end,end_vertices-1,end_vertices,edges_bool)

        ! Supprime les sommets de la boite englobante
        vertices_bool(end_vertices-3) = .false.
        vertices_bool(end_vertices-2) = .false.
        vertices_bool(end_vertices-1) = .false.
        vertices_bool(end_vertices) = .false.

        nb_vertices = nb_vertices-4
        end_vertices = end_vertices-4

        !triangles_list2=triangles_list
        triangles_bool2=triangles_bool
        nb_triangles2=nb_triangles
        Call delete_triangle(triangles_list,triangles_bool2,end_triangles,edges_list,&
                             &edges_end,edges_list(1),edges_list(2),edges_bool)

        nb_triangles=nb_triangles2

        Do i=1,end_triangles
          If(triangles_bool2(i)) Then
            triangles_bool(i)= .False.
            nb_triangles=nb_triangles-1
          End If
        End Do

    End Subroutine delete_out_of_border

    ! Prend une arete en entree, supprime le triangle correspondant si possible
    Recursive Subroutine delete_triangle(triangles_list,triangles_bool,end_triangles,edges_list,&
                                         &edges_end,edge1,edge2,edges_bool)

        Integer,Dimension(3*array_size),Intent(In)  :: triangles_list
        Logical,Dimension(array_size),Intent(Inout) :: triangles_bool,edges_bool
        Integer,Intent(In)                          :: end_triangles,edges_end,edge1,edge2
        Integer,Dimension(2*array_size),Intent(In)  :: edges_list
        Integer                                     :: s11,s12,s21,s22,ind,summit

        ind = find_triangle(edge1,edge2,triangles_list,triangles_bool,end_triangles)

        If(ind .NE. -1)Then
            triangles_bool(ind) = .false.
            nb_triangles = nb_triangles-1
            summit = find_summit(edge1,edge2,triangles_list,ind)
            If(is_border(edge1,summit,edges_list,edges_end,edges_bool) .EQ. -1)Then
                Call delete_triangle(triangles_list,triangles_bool,end_triangles,edges_list,&
                                     &edges_end,edge1,summit,edges_bool)
            End If
            If(is_border(edge2,summit,edges_list,edges_end,edges_bool) .EQ. -1)Then
                Call delete_triangle(triangles_list,triangles_bool,end_triangles,edges_list,&
                                     &edges_end,edge2,summit,edges_bool)
            End If
        End If

    End Subroutine delete_triangle

    ! Pour une arete donnee, force la frontiere si necessaire : swap
    Subroutine swap(s1,s2,triangles_list,triangles_bool,end_triangles)

        Integer,Intent(In)                            :: s1,s2
        Integer,Intent(Inout)                         :: end_triangles
        Integer,Dimension(3*array_size),Intent(Inout) :: triangles_list
        Logical,Dimension(array_size),Intent(Inout)   :: triangles_bool
        Integer                                       :: i,j,stri1,stri2,stri3,end_tri1,end_tri2
        Integer                                       :: st1,st2,ind_fin1,ind_fin2
        Integer,Dimension(30)                         :: ind_tri1,ind_tri2
        Logical                                       :: need_swap

        ! Initialisation de certaines variables
        need_swap = .true.
        ind_tri1 = 0
        ind_tri2 = 0
        end_tri1 = 0
        end_tri2 = 0

        ! Identifie si le swap est nécessaire
        Do i=1,end_triangles
            If(triangles_bool(i))Then
                stri1 = triangles_list(3*i-2)
                stri2 = triangles_list(3*i-1)
                stri3 = triangles_list(3*i)
                If(edge_triangle(s1,s2,stri1,stri2,stri3))Then
                    need_swap = .false.
                End If
            End If
        End Do

        ! Determine tout les triangle ayant pour sommet un des sommets de l'arete de bord
        If(need_swap)Then
            Do i=1,end_triangles
                If(triangles_bool(i))Then

                    stri1 = triangles_list(3*i-2)
                    stri2 = triangles_list(3*i-1)
                    stri3 = triangles_list(3*i)

                    If(vertice_triangle(s1,stri1,stri2,stri3))Then
                        ! Stocke l'indice du triangle et les deux sommets ordonnes autres que
                        ! ceux de l'arete
                        end_tri1 = end_tri1 + 1
                        ind_tri1(3*end_tri1-2) = i
                        Call two_vertices(s1,st1,st2,stri1,stri2,stri3)
                        ind_tri1(3*end_tri1-1) = st1
                        ind_tri1(3*end_tri1) = st2
                    End If
                    If(vertice_triangle(s2,stri1,stri2,stri3))Then
                        ! Stocke l'indice du triangle et les deux sommets ordonnes autres que
                        ! ceux de l'arete
                        end_tri2 = end_tri2 + 1
                        ind_tri2(3*end_tri2-2) = i
                        Call two_vertices(s2,st1,st2,stri1,stri2,stri3)
                        ind_tri2(3*end_tri2-1) = st1
                        ind_tri2(3*end_tri2) = st2
                    End If
                End If
            End Do
        End If

        ! Determine les deux sommets pour faire le swap
        Do i=1,end_tri1
            Do j=1,end_tri2
                If((ind_tri1(3*i-1).EQ.ind_tri2(3*j-1)).AND.(ind_tri1(3*i).EQ.ind_tri2(3*j)))Then
                    ! Suppression des triangles
                    triangles_bool(ind_tri1(3*i-2)) = .false.
                    triangles_bool(ind_tri2(3*j-2)) = .false.
                    ! Reconstruction des triangles
                    end_triangles = end_triangles + 1
                    triangles_bool(end_triangles) = .true.
                    triangles_list(3*end_triangles-2) = ind_tri1(3*i-1)
                    triangles_list(3*end_triangles-1) = s1
                    triangles_list(3*end_triangles) = s2
                    end_triangles = end_triangles + 1
                    triangles_bool(end_triangles) = .true.
                    triangles_list(3*end_triangles-2) = ind_tri1(3*i)
                    triangles_list(3*end_triangles-1) = s1
                    triangles_list(3*end_triangles) = s2
                End If
            End Do
        End Do

    End Subroutine swap

    ! Renvoie les deux sommets ordonnes autres que ceux de l'arete pour le swap
    Subroutine two_vertices(s,st1,st2,stri1,stri2,stri3)

        Integer,Intent(In)    :: stri1,stri2,stri3,s
        Integer,Intent(Inout) :: st1, st2

        If(s .EQ. stri1)Then
            If(stri2 .LT. stri3)Then
                st1 = stri2
                st2 = stri3
            Else
                st1 = stri3
                st2 = stri2
            End If
        ElseIf(s .EQ. stri2)Then
            If(stri1 .LT. stri3)Then
                st1 = stri1
                st2 = stri3
            Else
                st1 = stri3
                st2 = stri1
            End If
        Else
            If(stri1 .LT. stri2)Then
                st1 = stri1
                st2 = stri2
            Else
                st1 = stri2
                st2 = stri1
            End If
        End If

    End Subroutine

End Module border
