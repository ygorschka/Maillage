Module write_solution

    Use parameters
    Implicit None

Contains

    Subroutine write_mesh(vertices_list,end_vertices,triangles_list,end_triangles,&
                          &triangles_bool,edges_list,end_edges,name,vertices_bool,edges_bool)

        Real(Pr),Dimension(2*array_size),Intent(In) :: vertices_list
        Logical,Dimension(array_size),Intent(In)    :: triangles_bool,vertices_bool,edges_bool
        Integer,Intent(In)                          :: end_vertices,end_triangles,end_edges
        Integer,Dimension(3*array_size),Intent(In)  :: triangles_list
        Integer,Dimension(2*array_size),Intent(In)  :: edges_list
        Character(len=20),Intent(In)                :: name
        Integer                                     :: i

        Open(10, file=name, status = 'replace')

        Write(10,*) 'MeshVersionFormatted 1'
        Write(10,*) ' '
        Write(10,*) 'Dimension 2'
        Write(10,*) ' '
        Write(10,*) 'Vertices'
        Write(10,*) nb_vertices
        Do i=1,end_vertices
            If(vertices_bool(i))Then
                Write(10,*) vertices_list(2*i-1), vertices_list(2*i), 1
            End If
        End Do
        Write(10,*) ' '
        Write(10,*) 'Edges'
        Write(10,*) nb_edges
        Do i=1,end_edges
            If(edges_bool(i))Then
                Write(10,*) edges_list(2*i-1), edges_list(2*i), 1
            End If
        End Do
        Write(10,*) ' '
        Write(10,*) 'Triangles'
        Write(10,*) nb_triangles
        Do i=1,end_triangles
            If(triangles_bool(i))Then
                write(10,*) triangles_list(3*i-2), triangles_list(3*i-1), triangles_list(3*i), 1
            End If
        End Do
        Write(10,*) ' '
        Write(10,*) 'End'
        Close(10)

    End Subroutine write_mesh



    Subroutine write_quality(name, qualite)

        Real(Pr),Dimension(nb_triangles),Intent(In) :: qualite
        Character(len=20),Intent(In)                :: name
        Integer                                     :: i

        Open(10, file=name, status = 'replace')

        Write(10,*) 'MeshVersionFormatted 1'
        Write(10,*) ' '
        Write(10,*) 'Dimension 2'
        Write(10,*) ' '
        Write(10,*) 'SolAtTriangles'
        Write(10,*) nb_triangles
        Write(10,*) 1,1
        Do i=1,nb_triangles
          Write(10,*) qualite(i)
        End Do
        Write(10,*) ' '
        Write(10,*) 'End'
        Close(10)

    End Subroutine write_quality

End Module write_solution
