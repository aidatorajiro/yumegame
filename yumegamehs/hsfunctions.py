import itertools
import mathutils
from mathutils import *
import struct
import random
import bpy
import random
import bmesh
import json
from mathutils.bvhtree import BVHTree

RELLOC_BOTTOM_LEFT = mathutils.Vector((-0.7387151718139648, -0.37737855315208435, -0.9885141849517822))

boundary_table = {}

def reload_hsfunctions():
    """
    <itamname>はじめのおまじない</itamname>
    <itemdesc>おまじないとよく言われるが、そうはいっても、そこに絆を深める以上のものはあるのだろうか。結局、だれも機械の気持ちなんかわかっちゃいない。</itemdesc>
    """
    import os
    proj_path = bpy.path.abspath("//")
    print('Reloading hsfunctions...')
    with open(os.path.join(proj_path, "yumegamehs", "hsfunctions.py")) as f:
        exec(f.read(), globals())
    reset_distance_of_view()

def make_world_bvh(obj):
    """
    <itamname>おもいけいさんはおぼえておこう</itamname>
    <itemdesc>でも、時には無駄も悪くないのかもしれない。効率主義反対！</itemdesc>
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

def find_boundary(relpos=mathutils.Vector((0,-1, 0)), origin_diff=mathutils.Vector((0, 0.25, 0))):
    """
    <itamname>近国探知</itamname>
    <itemdesc>いまどこにいるかわからない。わからないから、何をしなければいいのかわからないし、何をしてはいけないのかもわからない。でもきっと官憲はやってくる。不安。そこで、両腕（あるいは首、あるいは足、あるいはお腹、あるいは肩、脇腹、）を伸ばし伸ばし伸ばしのばばばばばばばばし続けると、国境付近の検問所に着く。</itemdesc>
    find nearest boundary
    """
    r = get_region_3d()
    casts = raycast_boundary_view(relative_positions=[relpos], origin_diff=origin_diff)
    return max(
            itertools.chain(*[[(x[0], y[0][0]) for y in x[1]] for x in casts])
            , key=lambda x: (x[1] - r.view_location).length
        )[0]


def find_all_boundaries(relpos=mathutils.Vector((0,0,-1)), origin_diff=mathutils.Vector((0, 0, 0))):
    """
    <itamname>近国探知・改</itamname>
    <itemdesc></itemdesc>
    find all boundaries in sight (specify sight angle/position difference by relpos and origin_diff
    """
    casts = raycast_boundary_view(relative_positions=[relpos], origin_diff=origin_diff)
    return [c[0] for c in casts]

def raycast_view(obj, all=True, relative_positions=[mathutils.Vector((0,0,-1))], origin_diff=mathutils.Vector((0, 0, 0))):
    """
    <itamname>光線</itamname>
    <itemdesc>あたまから光線が降り注ぐ。あるいは眼の前から。あるいは光線を出しているのは私かもしれない。その光線を浴びると、なにかいいことが起こるかもしれないし、ひょっとしたら５億円があたるかもしれないし、悪いことが起こるかもしれない。でも知ることはできるのは一握りの人間だけで、たいてい光線を浴びせられる人は、一生知ることはできない。</itemdesc>
    raycast vector(s) that is/are relative to the viewpoint against a specified object
    in: obj (Object) object to be raycasted
        all (Bool) check for all boundaries
        relative_positions ([Vector]) vector(s) relative to the viewpoint
    out: (raycast result, matched relative position) if all=False
        or  [(raycast result, matched relative position)] if all=True

        the first string is the id of collection without hashtag
    """
    r = get_region_3d()
    results = []
    bvh = make_world_bvh(obj)
    for p in relative_positions:
        origin = r.view_location.copy()
        inv_mat = r.view_rotation.inverted().to_matrix()
        origin += origin_diff @ inv_mat
        result = bvh.ray_cast(origin, p @ inv_mat)
        if result[0] is not None:
            if all == False:
                return (result, p)
            else:
                results.append((result, p))
    return results

from json import JSONEncoder

class MyEncoder(JSONEncoder):
    def default(self, o):
        if isinstance(o, mathutils.Vector):
            return [o.x, o.y, o.z]
        return super().default(o)

def raycast_boundary_view(relative_positions=[mathutils.Vector((0,0,-1))], origin_diff=mathutils.Vector((0, 0, 0))):
    """
    <itamname></itamname>
    <itemdesc></itemdesc>
    raycast vector(s) relative to the viewpoint with registered boundaries
    in: all (Bool) check for all boundaries
        relative_positions ([Vector]) vector(s) relative to the viewpoint
    out: [(string, [(raycast result, matched relative position)])] 
        the first string is the id of collection without hashtag
    """
    results = []
    for c in filter(lambda x: x.name.startswith('#'), bpy.data.collections):
        objname = '#bound.' + c.name[1:]
        raycasts = raycast_view(bpy.data.objects[objname], relative_positions=relative_positions, origin_diff=origin_diff)
        if len(raycasts) > 0:
            results.append((c.name[1:], raycasts))
    return results

def calculate_relpos(pos):
    r = get_region_3d()
    return (pos - r.view_location) @ r.view_rotation.to_matrix()

# (0 +/- 2, -1 +/- 0, 0 +/- 2)
def choose_point_around():
    r = get_region_3d()
    objname = find_boundary(relpos=mathutils.Vector((0,-1, 0)), origin_diff=mathutils.Vector((0, 0.25, 0)))
    obj = bpy.data.objects['#main.' + objname]
    relpos = mathutils.Vector((random.random() * 4 - 2, -1, random.random() * 4 -2))
    origin_diff = mathutils.Vector((0, 1, 0))
    raycast = raycast_view(obj, relative_positions=[relpos], origin_diff=origin_diff)[0]
    origin = r.view_location.copy() + (origin_diff @ r.view_rotation.inverted().to_matrix())
    raycast_diff = raycast[0][0] - origin
    final_pos = origin + raycast_diff * 0.9
    return final_pos

def new_instance(name, location, scale, rotation, colname='@ayumi'):
    bpy.ops.object.collection_instance_add(collection=name, align='WORLD', location=location, scale=scale, rotation=rotation)
    for x in bpy.data.objects:
        if x.name.startswith(name):
            if not(len(x.users_collection) == 1 and x.users_collection[0] == bpy.data.collections[colname]):
                unlink_all_and_link(x, bpy.data.collections[colname])

def adjust_instance_offset():
    pass

def debug_choose_point_around():
    r = choose_point_around()
    copy_obj(bpy.data.objects['#template.debugsphere'], '#debugsphere', copy_data=False, loc=r, ignore_name_exists=True, collection=bpy.data.collections['@ayumi'])

def debug_choose_point_around_2():
    r = choose_point_around()
    r1 = random.random()
    r2 = random.random()
    r3 = random.random()
    new_instance('$hikarukanagu', (r.x, r.y, r.z), (1, 1, 1), (r1, r2, r3))

def unselect_all():
    for x in bpy.context.view_layer.objects.selected.values():
        x.select_set(False)

def copy_obj(obj, name, copy_data=False, loc=None, ignore_name_exists=False, collection=None):
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
    if collection is not None:
        unlink_all_and_link(o, bpy.data.collections['@ayumi'])
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
    copy_obj(bpy.data.objects['#template.torch'], '#torch', loc=r.view_location.copy(), ignore_name_exists=True, collection=bpy.data.collections['@ayumi'])

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
    d = json.dumps(data, cls=MyEncoder).encode()
    print(d)
    the_socket.send(struct.pack(">Q", len(d)) + d)

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
