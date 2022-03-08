Module quality

  Use parameters
  Use geometry

  Implicit None

Contains

  Subroutine elements_quality(vertices,triangles,triangles_bool,end_triangles,qualite)

    Real(PR),Dimension(2*array_size),Intent(In)     :: vertices
    Integer,Dimension(3*array_size),Intent(In)      :: triangles
    Logical,Dimension(array_size),Intent(In)        :: triangles_bool
    Integer,Intent(In)                              :: end_triangles
    Real(PR), Dimension(nb_triangles),Intent(InOut) :: qualite
    Integer                                         :: i, e
    Real(PR)                                        :: x1, y1, x2, y2, x3, y3, area, l1, l2, l3

    i=1
    Do e=1,end_triangles
      If(triangles_bool(e)) Then
        x1=vertices(2*triangles(3*e-2)-1)
        y1=vertices(2*triangles(3*e-2))
        x2=vertices(2*triangles(3*e-1)-1)
        y2=vertices(2*triangles(3*e-1))
        x3=vertices(2*triangles(3*e)-1)
        y3=vertices(2*triangles(3*e))

        area=triangle_area(x1,y1,x2,y2,x3,y3)

        l1=edge_length(x2,y2,x3,y3)
        l1=l1**2
        l2=edge_length(x1,y1,x3,y3)
        l2=l2**2
        l3=edge_length(x1,y1,x2,y2)
        l3=l3**2

        qualite(i)=(sqrt(3._PR)*(l1+l2+l3))/(12._PR*area)
        i=i+1
      End If
    End Do

  End Subroutine elements_quality

End Module quality
