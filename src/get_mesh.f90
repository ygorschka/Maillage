Module get_mesh

  Use parameters

  Implicit None

Contains

  ! Recupere le nombre d'aretes de bord et de sommets dans le fichier d'entre
  Subroutine get_nb(file_name, end_vertices, end_edges)

    Character(len=80),Intent(In) :: file_name
    Character(len=20)            :: charac
    Integer,Intent(Inout)        :: end_vertices, end_edges

    charac = 'Init'

    Open(10, file=file_name, status='old')

    Read(10,*)
    Read(10,*)
    Read(10,*)

    Do While(charac .Ne. 'End')

       Read(10,*) charac

       If(charac .Eq. 'Vertices')Then
          Read(10,*) end_vertices
       Else If(charac .Eq. 'Edges')Then
          Read(10,*) end_edges
       End If

    End Do

    Close(10)

  End Subroutine Get_nb

  ! Recupere les aretes et les sommets dans le fichier d'entre
  Subroutine get_elements(file_name,vertices,vertices_bool,edges,edges_bool)

    Real(Pr),Dimension(2*array_size),Intent(Inout) :: vertices
    Integer,Dimension(2*array_size),Intent(Inout)  :: edges
    Logical,Dimension(array_size),Intent(Inout)    :: vertices_bool,edges_bool
    Character(len=80),Intent(In)                   :: file_name
    Real(Pr)                                       :: s1, s2
    Integer                                        :: nb, i
    Character(len=20)                              :: charac

    charac = 'Init'

    Open(11, file=file_name, status='old')

    Do While(charac .Ne. 'End')

       Read(11,*) charac

       If(charac .Eq. 'Vertices')Then
          Read(11,*) nb
          Do i = 1,nb
             Read(11,*) s1, s2
             vertices(2*i-1) = s1
             vertices(2*i) = s2
             vertices_bool(i) = .true.
          End Do
       Else If(charac .Eq. 'Edges')Then
          Read(11,*) nb
          Do i = 1,nb
             Read(11,*) s1, s2
             edges(2*i-1) = s1
             edges(2*i) = s2
             edges_bool(i) = .true.
          End Do
       End If

    End Do

  End Subroutine get_elements

End Module get_mesh
