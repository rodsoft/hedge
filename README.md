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

