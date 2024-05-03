import mathutils
import struct
import random
import bpy

RELLOC_BOTTOM_LEFT = mathutils.Vector((-0.7387151718139648, -0.37737855315208435, -0.9885141849517822))

def change_text(obj, txt):
    obj.data.body = txt

def get_relative_location_to_view(obj):
    r = get_region_3d()
    l = obj.location -  r.view_location
    return r.view_rotation.inverted() @ l

def obj_from_name(name):
    return bpy.data.objects[name]

def align_to_camera(obj, relloc):
    r = get_region_3d()
    obj.rotation_quaternion = r.view_rotation.copy()
    obj.location = r.view_location + (r.view_rotation.copy() @ relloc)

def set_bg_color(r = 1.0, g = 0.6611027121543884, b = 0.0736767128109932, a = 1.0):
    i = bpy.data.worlds["World"].node_tree.nodes["Background"].inputs[0]
    if isinstance(i, bpy.types.NodeSocketColor) and i.type == 'RGBA':
        i,default_value[0] = r
        i,default_value[1] = g
        i,default_value[2] = b
        i,default_value[3] = a

def set_bg_strength(s):
    bpy.data.worlds["World"].node_tree.nodes["Background"].inputs[1].default_value = s

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
