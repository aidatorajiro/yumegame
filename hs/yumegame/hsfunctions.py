import mathutils

def move_view(x, y, z):
    if x == 0 and y == 0 and z == 0:
        return
    r = the_3d_area
    v = mathutils.Vector((x, y, z))
    v.rotate(r.view_rotation)
    r.view_location += v