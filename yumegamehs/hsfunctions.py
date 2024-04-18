import mathutils
import struct
import random

def debugprint(x):
    print(x)

def random_vector():
    offset = mathutils.Vector((random.random(), random.random(), random.random())) - mathutils.Vector((0.5, 0.5, 0.5))
    return offset

def place_torch_around():
    r = get_region_3d()
    offset = random_vector()
    copy = r.view_location.copy()
    copy += offset
    print("place_torch_around")

def save_blend():
    bpy.ops.wm.save_as_mainfile(filepath=bpy.data.filepath)

def get_collection():
    return bpy.data.collections.new("YumegameCollection")

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

def sock_send(data):
    the_socket.send(struct.pack(">Q", len(data)) + data)

def reset_distance_of_view():
    get_region_3d().view_distance = 0.1

def rotate_view(x, y, z):
    r = get_region_3d()
    v = mathutils.Euler((x, y, z), "XYZ")
    q = r.view_rotation.copy()
    q.invert()
    q.rotate(v)
    q.rotate(r.view_rotation)
    q2 = r.view_rotation.copy()
    q2.rotate(q)
    r.view_rotation = q2
