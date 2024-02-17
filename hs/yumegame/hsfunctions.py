import mathutils

def move_view(x, y, z):
    if x == 0 and y == 0 and z == 0:
        return
    r = the_3d_area
    v = mathutils.Vector((x, y, z))
    v.rotate(r.view_rotation)
    r.view_location += v

def reset_distance_of_view():
    the_3d_area.view_distance = 0.0

def rotate_view(x, y, z):
    print(x, y, z, 'rotate')
    r = the_3d_area
    v = mathutils.Euler((x, y, z), "XYZ")
    q = r.view_rotation.copy()
    q.invert()
    q.rotate(v)
    q.rotate(r.view_rotation)
    r.view_rotation.rotate(q)