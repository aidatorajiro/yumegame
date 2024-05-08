import mathutils
import struct
import random
import bpy
import bmesh
from mathutils.bvhtree import BVHTree

RELLOC_BOTTOM_LEFT = mathutils.Vector((-0.7387151718139648, -0.37737855315208435, -0.9885141849517822))

boundary_table = {}

def make_world_bvh(obj):
    """
    create bvh object adjusted to world's absolute coordinate system
    in: object
    out: bvh
    """
    if not obj.name in boundary_table:
        o = bpy.data.objects[obj.name]
        bm = bmesh.new()
        bm.from_mesh(o.data)
        bm.transform(o.matrix_world)
        bvh = BVHTree.FromBMesh(bm)
        boundary_table[obj.name] = bvh
    else:
        bvh = boundary_table[obj.name] 
    return bvh

def raycast_boundary_view(all=True, relative_positions=[mathutils.Vector((0,0,-1))]):
    """
    raycast vector(s) relative to the viewpoint with registered boundaries
    in: all (Bool) check for all boundaries
        relative_positions ([Vector]) vector(s) relative to the viewpoint
    out: (string, raycast result) if all=False
        or  [(string, raycast result)] if all=True

        the first string is the id of collection without hashtag
    """
    r = get_region_3d()
    results = []
    for c in filter(lambda x: x.name.startswith('#'), bpy.data.collections):
        objname = '#bound.' + c.name[1:]
        bvh = make_world_bvh(bpy.data.objects[objname])
        for p in relative_positions:
            result = bvh.ray_cast(r.view_location.copy(), p @ r.view_rotation.inverted().to_matrix())
            if result[0] is not None:
                if all == False:
                    return (bpy.data.objects[objname], result)
                else:
                    results.append((c.name[1:], result))
    return results

def unselect_all():
    for x in bpy.context.view_layer.objects.selected.values():
        x.select_set(False)

def copy_obj(obj, name, copy_data=False, loc=None, ignore_name_exists=False):
    if not name in bpy.data.objects or ignore_name_exists:
        o = obj.copy()
        o.name = name
    else:
        raise ValueError('ERROR: object name already exists')
    if loc:
        o.location = loc
    if copy_data:
        o.data = o.data.copy
    bpy.context.collection.objects.link(o)
    return o

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

def unlink_all_and_link(obj, col):
    for x in obj.users_collection:
        x.objects.unlink(obj)
    col.objects.link(obj)

def place_torch_around():
    r = get_region_3d()
    o = copy_obj(bpy.data.objects['#template.torch'], '#torch', loc=r.view_location.copy(), ignore_name_exists=True)
    unlink_all_and_link(o, bpy.data.collections['@ayumi'])

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
    if not 0.099 < get_region_3d().view_distance < 0.101:
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
