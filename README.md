hedge
=====

Half-Edge data structure in Lua

struct Vertex
{
  int id;
};

struct Edge
{
  Face *face;
  Edge *prev, *next;
  Edge *opp; -- opposite edge
  Vertex *vtx;
};

struct Face
{
  int id;
  Edge *edge;
};

struct Mesh
{
    Vertex vertices[];
    Faces  faces[];
    Edges  edges[];
};

Usage:

mesh = Mesh:new()

f1 = mesh:add_face(1,2,3)
--[[  result
        3
        /\
       /  \
      /    \
     /      \
    /        \
   /          \
  /____________\
 1             2
]]

for e in f1:edges() do
  print(e.vtx.id,'->',e.next.vtx.id)
end
-- output:
1 -> 2
2 -> 3
3 -> 1


f2 = mesh:add_face(3,2,4)
--[[  result:
        3______________4
        /\            / 
       /  \          /
      /    \        /
     /      \      /
    /        \    /
   /          \  /
  /____________\/
 1             2
]]

v5 = mesh:split_edge(mesh:get_edge(3,2), 5)

--[[  result:
        3______________4
        /\            / 
       /  \          /
      /    \        /
     /      *5     /
    /        \    /
   /          \  /
  /____________\/
 1             2
]]


-- loop through all out edges of vertex 5:
for e in v5:out_edges() do
  print(e.vtx.id,'->',e.next.vtx.id)
end
-- output:
5 -> 2
5 -> 3

mesh:remove_vertex(5)
-- [[ result:

        3______________4
        /             / 
       /             /
      /             /
     /             /
    /             /
   /             /
  /_____________/
 1             2
]]
