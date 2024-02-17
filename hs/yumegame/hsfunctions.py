import mathutils

def get_region_3d():
    for a in bpy.context.screen.areas:
        if a.spaces.active and a.spaces.active.type == 'VIEW_3D':
            return a.spaces.active.region_3d

def move_view(x, y, z):
    if x == 0 and y == 0 and z == 0:
        return
    r = get_region_3d()
    v = mathutils.Vector((x, y, z))
    v.rotate(r.view_rotation)
    r.view_location += v

def reset_distance_of_view():
    get_region_3d().view_distance = 0.1

def rotate_view(x, y, z):
    print(x, y, z, 'rotate')
    r = get_region_3d()
    v = mathutils.Euler((x, y, z), "XYZ")
    q = r.view_rotation.copy()
    q.invert()
    q.rotate(v)
    q.rotate(r.view_rotation)
    q2 = r.view_rotation.copy()
    q2.rotate(q)
    r.view_rotation = q2