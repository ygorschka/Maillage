Program main

  Use border
  Use delaunay
  Use get_mesh
  Use parameters
  Use quality
  Use subdivision
  Use write_solution

  Implicit None

  Integer                             :: end_vertices, end_edges, end_triangles, i, i1, i2, i3
  Character(len=80)                   :: file_name
  Real(Pr),Dimension(2*array_size)    :: vertices
  Real(Pr),Dimension(:),Allocatable   :: qualite
  Logical,Dimension(array_size)       :: vertices_bool, triangles_bool, edges_bool
  Integer,Dimension(3*array_size)     :: triangles
  Integer,Dimension(2*array_size)     :: edges
  Real(Pr),Dimension(2)               :: center
  Logical                             :: bool
  Character(len=20)                   :: name

  ! Fichier d'entree
  file_name = './Test_cases/de.mesh'

  ! Initialisation de certaines variables
  end_triangles = 0
  vertices = 0
  vertices_bool = .false.
  triangles_bool = .false.
  edges_bool = .false.

  Call get_nb(file_name, end_vertices, end_edges)
  nb_vertices = end_vertices
  nb_edges = end_edges

  Call get_elements(file_name, vertices, vertices_bool, edges, edges_bool)

  ! Triangulation du domaine
  ! Creation de la boite englobante
  Call box(vertices, vertices_bool, end_vertices, end_edges, edges)

  ! Ajout des 2 triangles initiaux
  Call add_triangle(triangles,triangles_bool,end_triangles,end_vertices-3,end_vertices-2,&
                    &end_vertices-1)
  Call add_triangle(triangles,triangles_bool,end_triangles,end_vertices-3,end_vertices-1,&
                    &end_vertices)

  ! Triangulation de la boite englobante
  Do i=1,end_vertices-4
    Call delaunay_core(triangles,triangles_bool,vertices,vertices_bool,end_triangles,end_vertices,i)
  End Do

  ! Forcage de la frontiere
  Do i=1,end_edges
      Call swap(edges(2*i-1),edges(2*i),triangles,triangles_bool,end_triangles)
  End Do

  ! Suppression de l'exterieur
  Call delete_out_of_border(triangles,triangles_bool,end_triangles,edges,end_edges,vertices_bool,&
                            &end_vertices,edges_bool)

  ! Subdivision des triangles
  Do i=1,nb_subdivision
      Call subdivise(triangles,triangles_bool,edges,edges_bool,vertices,vertices_bool,end_triangles,&
                     &end_edges,end_vertices)
  End Do

  ! Sauvegarde de la solution
  name = 'result.mesh'
  Call write_mesh(vertices,end_vertices,triangles,end_triangles,triangles_bool,edges,end_edges,&
                  &name,vertices_bool,edges_bool)

  ! Qualite des elements du maillage
  name = 'result.sol'
  Allocate(qualite(1:nb_triangles))
  Call elements_quality(vertices,triangles,triangles_bool,end_triangles,qualite)
  Call write_quality(name, qualite)
  Deallocate(qualite)

End Program main
