Module subdivision

    Use geometry
    Use parameters

    Implicit None

Contains

    ! Subdivise tout les triangles du domaine
    Subroutine subdivise(triangles_list,triangles_bool,edges_list,edges_bool,&
                         &vertices_list,vertices_bool,end_triangles,end_edges,end_vertices)

        Integer,Dimension(3*array_size),Intent(Inout)  :: triangles_list
        Logical,Dimension(array_size),Intent(Inout)    :: triangles_bool,edges_bool,vertices_bool
        Integer,Dimension(2*array_size),Intent(Inout)  :: edges_list
        Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices_list
        Integer,Intent(Inout)                          :: end_triangles,end_edges,end_vertices
        Integer                                        :: ind,middle,edge1,edge2
        Real(Pr)                                       :: x1,y1

        ! Determine l'indice de l'arete initiale
        ind = 1
        Do While(.NOT. edges_bool(ind))
            ind = ind+1
        End Do

        ! Ajout du premier point-milieu
        edge1 = edges_list(2*ind-1)
        edge2 = edges_list(2*ind)
        Call middle_edge(edge1,edge2,vertices_list,x1,y1)
        Call add_vertice(vertices_list,vertices_bool,end_vertices,x1,y1)
        middle = nb_vertices

        Call subdivise_triangle(triangles_list,triangles_bool,edges_list,edges_bool,&
                                &vertices_list,vertices_bool,edge1,edge2,middle,&
                                &end_triangles,end_edges,end_vertices)

    End Subroutine subdivise

    Recursive Subroutine subdivise_triangle(triangles_list,triangles_bool,edges_list,edges_bool,&
                                            &vertices_list,vertices_bool,edge1,edge2,middle,&
                                            &end_triangles,end_edges,end_vertices)

        Integer,Dimension(3*array_size),Intent(Inout)  :: triangles_list
        Logical,Dimension(array_size),Intent(Inout)    :: triangles_bool,edges_bool,vertices_bool
        Integer,Dimension(2*array_size),Intent(Inout)  :: edges_list
        Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices_list
        Integer,Intent(Inout)                          :: end_triangles,end_edges,end_vertices
        Integer,Intent(In)                             :: middle,edge1,edge2
        Integer                                        :: ind,summit,ind_edge,ind_n2,ind_n3
        Real(Pr)                                       :: x1,y1

        ind = find_triangle(edge1,edge2,triangles_list,triangles_bool,end_triangles)

        ! Suppression et ajout d'arete de bord si necessaire
        ind_edge = is_border(edge1,edge2,edges_list,end_edges,edges_bool)
        If(ind_edge .NE. -1)Then
            edges_bool(ind_edge) = .false.
            end_edges = end_edges+1
            edges_bool(end_edges) = .true.
            edges_list(2*end_edges-1) = edge1
            edges_list(2*end_edges) = middle
            end_edges = end_edges+1
            edges_bool(end_edges) = .true.
            edges_list(2*end_edges-1) = edge2
            edges_list(2*end_edges) = middle
            nb_edges = nb_edges+1
        End If

        If(ind .NE. -1)Then
            summit = find_summit(edge1,edge2,triangles_list,ind)
            ! Suppression du triangle
            triangles_bool(ind) = .false.
            nb_triangles = nb_triangles-1
            ! Ajout d'un nouveau point
            Call middle_edge(edge1,summit,vertices_list,x1,y1)
            Call add_vertice(vertices_list,vertices_bool,end_vertices,x1,y1)
            ind_n2 = nb_vertices
            ! Ajout de l'autre point
            Call middle_edge(edge2,summit,vertices_list,x1,y1)
            Call add_vertice(vertices_list,vertices_bool,end_vertices,x1,y1)
            ind_n3 = nb_vertices
            ! Ajout des triangles
            Call add_triangle(triangles_list,triangles_bool,end_triangles,middle,ind_n2,ind_n3)
            Call add_triangle(triangles_list,triangles_bool,end_triangles,edge2,middle,ind_n3)
            Call add_triangle(triangles_list,triangles_bool,end_triangles,edge1,middle,ind_n2)
            Call add_triangle(triangles_list,triangles_bool,end_triangles,summit,ind_n2,ind_n3)
            ! Appel pour ajouter recursivement
            Call subdivise_triangle(triangles_list,triangles_bool,edges_list,edges_bool,&
                                    &vertices_list,vertices_bool,edge1,summit,ind_n2,&
                                    &end_triangles,end_edges,end_vertices)
            Call subdivise_triangle(triangles_list,triangles_bool,edges_list,edges_bool,&
                                    &vertices_list,vertices_bool,edge2,summit,ind_n3,&
                                    &end_triangles,end_edges,end_vertices)

        End If

    End Subroutine subdivise_triangle

End Module subdivision
