-- Mesh structure using half-edges
-- Author: Rodolfo Lima
-- Creation:    2014/03/14
-- Last update: 2014/03/16

local M = {}

M.setmetatable = setmetatable
M.getmetatable = getmetatable
M.type = type
M.ipairs = ipairs
M.pairs = pairs
M.tostring = tostring
M.assert = assert
M.print = print
M.rawget = rawget
M.io = io
M.os = os
M.table = table

_ENV = M

-- VERTEX -----------------------------------------------------------------

Vertex = {} -- { id = nil, edge = nil }
function Vertex:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Vertex:__eq(v)
    if is_vertex(v) then
        return self.id == v.id
    else
        return self.id == v
    end
end

function Vertex:__tostring()
    return "Vertex #"..tostring(self.id)
end

function is_vertex(v)
    return type(v)=="table" and getmetatable(v)==Vertex
end

function Vertex:check()
    assert(self.edge ~= nil, "vertex must have an edge")
    assert(self.edge.vtx == self, "Bad edge <-> vtx link")
end

function Vertex:out_edges()
    local e = self.edge
    return function()
        if e == nil or e.opp == nil then
            return nil
        end

        local cur = e
        e = e.opp.next
        if e == self.edge then
            e = nil
        end

        return cur
    end
end

function Vertex:in_edges()
    local out = self:out_edges()
    return function()
        local o = out()
        if o == nil then
            return nil
        else
            return o.opp -- will be an in-edge
        end
    end
end

-- EDGE -----------------------------------------------------------------

Edge = {} -- { vtx = nil, face = nil, next = nil, prev = nil, opp = nil }

function Edge:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end
function Edge.__eq(a,b)
    return a.id == b.id
end
function Edge:key()
    if self.next == nil then
        return tostring(self.vtx.id)..">nil"
    else
        return tostring(self.vtx.id)..">"..tostring(self.next.vtx.id)
    end
end
function Edge:__tostring()
    return "Edge #"..self.id.." "..self:key()
end
function is_edge(e)
    return type(e)=="table" and getmetatable(e)==Edge
end

function Edge:check(check_opp)
    assert(self.id ~= nil, "edge must have an id")
    assert(self.opp ~= nil, "edge must have an opposite edge")
    assert(self.next ~= nil, "edge must have a next edge")
    assert(self.prev ~= nil, "edge must have a prev edge")

    assert(self.next ~= self, "edge's next edge cannot be itself")
    assert(self.prev ~= self, "edge's prev edge cannot be itself")

    check_opp = check_opp==nil and true or check_opp

    assert(self.next.prev == self, tostring(self)..": bad edge forward link")
    assert(self.prev.next == self, tostring(self)..": bad edge backward link")
    assert(self.opp.opp == self,
        tostring(self)..": bad opposite edge link ("..tostring(self.opp.opp).." != "..tostring(self)..")")

    assert(self.face == self.next.face, tostring(self)..": adjacent edges must belong to the same face")
    assert(self.face == self.prev.face, tostring(self)..": adjacent edges must belong to the same face")

    self.vtx:check()

    if check_opp then
        self.opp:check(false)
    end
end

-- FACE -----------------------------------------------------------------

Face = {} -- { id = nil, edge = nil }
function Face:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = function(f,key)
        if type(key) == "number" then
            local first = f.edge
            local e = first
            for i=2,key do
                e = e.next
                if e == first then
                    return nil
                end
            end

            return e
        else
            return rawget(f,key) or rawget(Face,key)
        end
    end
    return o
end
function Face:__tostring()
    local str = "Face #"..self.id
    return str
end
function Face:__eq(that)
    return self.id == that.id
end
function Face:edges()
    local e = self.edge
    return function()
        if e == nil then
            return nil
        end

        local cur = e
        e = e.next
        if e == self.edge then
            e = nil
        end

        return cur
    end
end

function Face:check()
    assert(self.edge ~= nil, "face must have an edge")

    for e in self:edges() do
        e:check()
        assert(e.face == nil or e.face == self, tostring(e)..": bad edge face, "..tostring(self).." != "..tostring(e.face))
    end
end

-- MESH ------------------------------------------------------------------

Mesh = {} -- { faces = {}, vertices = {}, edges = {}}

function Mesh:new(o)
    o = o or {faces = {}, vertices = {}, edges = {}}
    setmetatable(o, self)

    setmetatable(o.edges, {__mode="v"})
    setmetatable(o.vertices, {__mode="v"})

    o.idnewface = 1
    o.idnewedge = 1

    self.__index = self
    return o
end

function Mesh:get_vertex(id)
    if type(id) == "number" then
        return self.vertices[id]
    elseif is_vertex(id) then
        return id
    elseif is_edge(id) then
        return id.vtx
    else
        return self.vertices[tostring(id)]
    end
end

function Mesh:add_vertex(id)
    assert(id ~= nil)
    local vtx = Vertex:new{id = id}

    if type(id) == "number" then
        self.vertices[id] = vtx
    else
        self.vertices[tostring(id)] = vtx
    end

    return vtx
end

function Mesh:_add_edge(e)
    assert(e.vtx ~= nil, "edge must have a valid src vertex")
    assert(e.next.vtx ~= nil, "edge must have a valid target vertex")
    assert(e.vtx ~= e.next.vtx, "singular edges not allowed")

    if e.id == nil then
        e.id = self.idnewedge
        self.idnewedge = self.idnewedge+1
    end

    self.edges[e:key()] = e
    return e
end
function Mesh:_remove_edge(e)
    self.edges[e:key()] = nil
end

function Mesh:get_or_add_vertex(id)
    local vtx = self:get_vertex(id)

    -- vertex not found? it's new
    if vtx == nil then
        return self:add_vertex(id),true
    else
        return vtx,false
    end
end

function Mesh:get_edge(v1, v2)
    v1 = self:get_vertex(v1)
    v2 = self:get_vertex(v2)

    local sv1 = v1 and tostring(v1.id) or 'nil'
    local sv2 = v2 and tostring(v2.id) or 'nil'

    return self.edges[sv1..'>'..sv2]
end

function Mesh:edge_exists(e)
    return self.edges[e:key()] ~= nil
end

function Mesh:create_face()
    local face = Face:new{id = self.idnewface}
    self.idnewface = self.idnewface+1
    self.faces[face.id] = face
    return face
end
function Mesh:remove_face(f)
    self.faces[f.id] = nil
end

function Mesh:add_face(...)
    local ids = {...}

    local face = self:create_face()

    local vertices = {}
    for v=1,#ids do
        vertices[v] = self:get_or_add_vertex(ids[v])
    end

    local edges = {}

    -- create edges
    for id,v1 in ipairs(vertices) do
        local edge = Edge:new{vtx = v1, face = face}

        -- new vertex? set its edge
        v1.edge = v1.edge or edge
        -- first edge of face? set its edge
        face.edge = face.edge or edge

        edges[#edges+1] = edge
    end

    -- create face edge cycle links
    for id,e1 in ipairs(edges) do
        local e2 = edges[id % #edges + 1]

        e1.next = e2
        e2.prev = e1

        self:_add_edge(e1)
    end

    -- create opposite edges
    for id,edge in ipairs(edges) do
        edge.opp = self:get_edge(edge.next.vtx, edge.vtx)
        if edge.opp == nil then
            edge.opp = Edge:new({vtx = edge.next.vtx, opp = edge, face = nil})
            edge.next.vtx.edge = edge.next.vtx.edge or edge.opp
        else
            edge.opp.opp = edge
        end
    end

    -- create opposite edges links
    for id,edge in ipairs(edges) do
        if edge.opp.face == nil then
            self:_remove_edge(edge.opp)

            edge.opp.prev = edge.next.opp
            edge.opp.next = edge.prev.opp

            self:_add_edge(edge.opp)
        end
    end

    -- fix opposite edges links
    for id,edge in ipairs(edges) do
        if edge.opp.face == nil then
            -- wrong prev?
            if edge.opp.prev.face ~= nil then
                -- go around dst vertex to find a border edge
                local bedge = edge.next.opp
                while bedge.face ~= nil do
                    bedge = bedge.next.opp
                end

                self:_remove_edge(bedge)

                edge.opp.prev = bedge
                bedge.next = edge.opp

                self:_add_edge(bedge)
            end

            -- wrong next?
            if edge.opp.next.face ~= nil then
                -- go around src vertex to find a border edge
                local bedge = edge.prev.opp
                while bedge.face ~= nil do
                    bedge = bedge.prev.opp
                end

                self:_remove_edge(edge.opp)

                edge.opp.next = bedge
                bedge.prev = edge.opp

                self:_add_edge(edge.opp)
            end
        end
    end

    return face
end

-- split face into face A (prev.face==A) and B (will be created)
function Mesh:add_edge(v1, v2, prev, next, reuse_face)
    reuse_face = reuse_face == nil and true or reuse_face

    assert(v1 ~= v2, "vertices must be different")

    v1 = self:get_or_add_vertex(v1)
    v2 = self:get_or_add_vertex(v2)

    -- disconnected, faceless edge?
    if v1.edge == nil and v2.edge == nil then
        assert(prev == nil, "prev edges is useless when new edge is disconnected")
        v1.edge = Edge:new{vtx = v1, face = nil}
        v2.edge = Edge:new{vtx = v2,
                           face = nil,
                           prev = v1.edge,
                           next = v1.edge,
                           opp = v1.edge
                       }
        v1.edge.prev = v2.edge
        v1.edge.next = v2.edge
        v1.edge.opp = v2.edge

        self:_add_edge(v1.edge)
        self:_add_edge(v2.edge)

        return v1.edge
    -- semi disconnected edge?
    elseif v1.edge == nil or v2.edge == nil then

        local a,b = v1.edge ~= nil and v1,v2 or v2,v1
        -- vertex a has an edge

        assert(prev.next.vtx == a, "invalid prev")
        
        local orig_edge = a.edge

        a.edge = Edge:new{vtx = a,
                          face = prev.face,
                          prev = prev}

        b.edge = Edge:new{vtx = b,
                           face = prev.face,
                           prev = a.edge,
                           next = prev.next,
                           opp = a.edge
                       }
        a.edge.opp = b.edge
        a.edge.next = b.edge

        prev.next.prev = b.edge
        prev.next = a.edge

        self:_add_edge(a.edge)
        self:_add_edge(b.edge)
        return a.edge
    end

    -- called as add_edge(v1,v2,prev,reuse_face) ?
    if type(next) == "boolean" then
        reuse_face = next
        next = nil
    end

    if next == nil then
        next = prev
        while next.vtx ~= v2 do
            next = next.prev
        end
    end

    assert(prev.next.vtx == v1, "invalid prev")
    assert(next.vtx == v2, "invalid next")

    assert(prev.face == next.face, "next and prev must belong to the same face")
    assert(prev.face ~= nil, "prev and next cannot be border edges")

    local face
    local old_face

    if not reuse_face then
        old_face = prev.face
        face = self:create_face()
        -- update edge faces with new face
        local e = next
        repeat
            e.face = face
            e = e.next
        until e == prev
        prev.face = face
    else
        face = prev.face
    end

    local opp_prev = next.prev
    local opp_next = prev.next

    local ne = Edge:new{vtx = v1,
                        face = face,
                        prev = prev,
                        next = next}
    self:_add_edge(ne)
    ne.prev.next = ne
    ne.next.prev = ne

    if v1.edge == nil then
        v1.edge = ne
    end

    assert(prev.face == ne.face)
    assert(next.face == ne.face)

    if face.edge == nil then
        face.edge = ne
    end

    ne.opp = Edge:new{vtx = v2,
                      face = self:create_face(),
                      prev = opp_prev,
                      next = opp_next,
                      opp = ne}
    self:_add_edge(ne.opp)
    opp_next.prev = ne.opp
    opp_prev.next = ne.opp

    ne.opp.face.edge = ne.opp

    if v2.edge == nil then
        v2.edge = ne.opp
    end

    -- update edge faces with new face
    local e = opp_next
    repeat
        -- if the face A edge was in face B, set it to an edge of face A
        if face.edge == e then
            face.edge = ne
        end
        e.face = ne.opp.face
        e = e.next
    until e == ne.opp

    if old_face ~= nil then
        self:remove_face(old_face)
    end

    return ne
end

function Mesh:remove_edge(edge)
    if not self:edge_exists(edge) then
        return false
    end

    if edge.opp.face == nil then
        edge = edge.opp
    end

    -- fix vertex edges
    if edge.vtx.edge == edge then
        edge.vtx.edge = edge.opp.next
    end

    if edge.opp.vtx.edge == edge.opp then
        edge.opp.vtx.edge = edge.next
    end

    -- remove edge.opp.face
    if edge.opp.face ~= nil and edge.face ~= edge.opp.face then
        self:remove_face(edge.opp.face)
    end

    local e = edge.opp.next
    while e ~= edge.opp do
        e.face = edge.face
        e = e.next
    end

    -- fix face edge
    if edge.face ~= nil and edge.face.edge == edge then
        edge.face.edge = edge.next
    end

    self:_remove_edge(edge)
    self:_remove_edge(edge.opp)

    if edge.vtx.edge == edge or edge.vtx.edge == edge.opp then
        self.vertices[edge.vtx.id] = nil
    end

    if edge.opp.vtx.edge == edge or edge.opp.vtx.edge == edge.opp then
        self.vertices[edge.opp.vtx.id] = nil
    end

    -- fix edge links
    edge.next.prev = edge.opp.prev
    edge.opp.prev.next = edge.next

    edge.prev.next = edge.opp.next
    edge.opp.next.prev = edge.prev

    return true
end

-- adds a vertex inside the face, create n faces inside input n-sided face
function Mesh:split_face(face, v, reuse_face)
    assert(self:get_vertex(v) == nil, "Must split face using a new vertex")

    reuse_face = reuse_face == nil and true or reuse_face

    v = self:add_vertex(v)

    -- save input face edges
    local edges = {}
    for e in face:edges() do
        edges[#edges+1] = e
    end

    local orig_face = face

    -- add new edges
    for _,e in ipairs(edges) do
        if not reuse_face or e ~= face.edge then
            face = self:create_face()
            face.edge = e
        end

        -- reuse original edge #1 (v1->v2)
        e.face = face
        self:_remove_edge(e)

        -- add edge #2 (v2->v)
        e.next = Edge:new({vtx = e.next.vtx, face = face, prev = e})

        -- add edge #3 (v->v3)
        e.next.next = Edge:new({vtx = v, face = face,
                                next = e, prev = e.next})

        if v.edge == nil then
            v.edge = e.next.next
        end

        -- set edge #1's prev to edge #3
        e.prev = e.next.next

        self:_add_edge(e)
        self:_add_edge(e.next)
        self:_add_edge(e.next.next)
    end

    -- fix opp edges
    local pe = edges[#edges]

    for i = 1,#edges do
        local ne = edges[i%#edges+1]
        local e = edges[i]

        e.next.opp = ne.prev
        assert(e.next.opp.face == ne.prev.face)
        e.prev.opp = pe.next
        assert(e.prev.opp.face == pe.next.face)

        pe = e
    end

    if not reuse_face then
        self:remove_face(orig_face)
    end

    return v
end

-- triangulate edge.face creating a fan around edge.vtx
function Mesh:triangulate(edge, reuse_face)
    local v = edge.vtx
    local prev_edge = edge.prev.prev.prev
    local next_edge = edge
    while prev_edge ~= next_edge do
        local e = self:add_edge(prev_edge.next.vtx, v,
                                prev_edge, next_edge, reuse_face)
        prev_edge = prev_edge.prev
    end
end

function Mesh:split_edge(edge, v, reuse_face)
    assert(self:get_vertex(v) == nil, "Must split edge using a new vertex")

    -- first we'll split the edge

    v = self:add_vertex(v)

    local edges = {}
    for e in edge.face:edges() do
        edges[#edges+1] = e
    end

    -- split edge
    edge.prev = Edge:new{vtx = edge.vtx,
                         face = edge.face,
                         prev = edge.prev,
                         next = edge}
    if edge.vtx.edge == edge then
        edge.vtx.edge = edge.prev
    end

    edge.prev.prev.next = edge.prev

    edge.vtx = v
    v.edge = edge

    self:_add_edge(edge.prev)

    -- split opp edge
    edge.opp.next = Edge:new{vtx = v,
                             face = edge.opp.face,
                             prev = edge.opp,
                             next = edge.opp.next,
                             opp = edge.prev}
    edge.opp.next.next.prev = edge.opp.next

    self:_add_edge(edge.opp.next)

    -- fix opp links
    edge.opp.opp = edge
    edge.prev.opp = edge.opp.next

    -- then we create triangles on edge's face, linking the added vertex to
    -- each vertex (minus 2)

    if edge.face ~= nil then
        self:triangulate(edge, reuse_face)
    end

    -- same thing on edge.opp.face

    if edge.opp.face ~= nil then
        self:triangulate(edge.opp.next, reuse_face)
    end

    return v
end

function Mesh:flip_edge(edge)
    -- if it's not an internal edge, there's no point in flipping it
    if edge.face == nil or edge.opp.face == nil then
        return false
    end

    -- change vertices

    -- if vertex points to edge, make it point to another edge with
    -- vertex as source
    if edge.vtx.edge == edge then
        edge.vtx.edge = edge.opp.next
    end
    edge.vtx = edge.prev.vtx

    if edge.opp.vtx.edge == edge.opp then
        edge.opp.vtx.edge = edge.next
    end
    edge.opp.vtx = edge.opp.prev.vtx

    -- change faces

    -- if face points to edge, make it point to other edge in face
    -- that won't have its face changed
    if edge.prev.face.edge == edge.prev then
        edge.prev.face.edge = edge
    end
    edge.prev.face = edge.opp.face

    if edge.opp.prev.face.edge == edge.opp.prev then
        edge.opp.prev.face.edge = edge.opp
    end
    edge.opp.prev.face = edge.face

    -- change next and prev links

    edge.prev.prev.next = edge
    edge.opp.prev.prev.next = edge.opp

    edge.next.prev = edge.opp.prev
    edge.opp.next.prev = edge.prev

    local edge_prev_prev = edge.prev.prev
    local edge_opp_prev_prev = edge.opp.prev.prev

    edge.prev.prev = edge.opp
    edge.opp.prev.prev = edge

    edge.prev.next = edge.opp.next
    edge.opp.prev.next = edge.next

    edge.next = edge.opp.prev
    edge.opp.next = edge.prev

    edge.prev = edge_prev_prev
    edge.opp.prev = edge_opp_prev_prev

    return true
end

function Mesh:remove_vertex(vtx)
    vtx = self:get_vertex(vtx)
    if vtx == nil then
        return false
    end

    local is_border_vertex = false

    -- save vertex edges before changing them
    local edges = {}
    for e in vtx:out_edges() do
        -- remove edges now since their vertices weren't changed yet
        self:_remove_edge(e)
        self:_remove_edge(e.opp)

        edges[#edges+1] = e

        if e.face == nil then
            is_border_vertex = true
        end
    end

    -- if it's a border vertex, all faces touching the vertex will be removed
    local face = not is_border_vertex and edges[1].face or nil

    -- remove edges around vertex
    for i = 1,#edges do
        local e = edges[i]

        -- not vertex face?
        -- remove the face (it if exists, i.e., edge is not a border)
        if e.face ~= face and e.face ~=nil then
            -- vertex points to the face that will be removed?
            if e.next.vtx.edge.face == e.face then
                -- make it point to the face that will remain
                e.next.vtx.edge.face = face
            end

            self:remove_face(e.face)
        end

        self:_remove_edge(e.opp.prev)

        -- fix edge links
        e.next.prev = e.opp.prev
        e.opp.prev.next = e.next

        -- vertex edge points to the edge that will not be removed
        e.next.vtx.edge = e.next

        self:_add_edge(e.opp.prev)

        e.next.face = face
    end

    -- remove vertex from our list of vertices
    self.vertices[vtx.id] = nil

    return true
end

function Mesh:face_count()
    local count = 0
    for _ in pairs(self.faces) do
        count = count+1
    end
    return count
end

function Mesh:vertex_count()
    local count = 0
    for _ in pairs(self.vertices) do
        count = count+1
    end
    return count
end

function Mesh:edge_count()
    local count = 0
    for _ in pairs(self.edges) do
        count = count+1
    end
    return count
end
        
function Mesh:output_dot(out)
    out = out or io.stdout

    out:write("digraph mesh {\n")

    function conv(v)
        return tostring(v):gsub('-','_')
    end

    -- we must guarantee output order, so sort edges on edge.id
    local sorted_edges = {}
    for _,e in pairs(self.edges) do
        sorted_edges[#sorted_edges+1] = e
    end
    table.sort(sorted_edges, function(a,b) return a.id < b.id end)

    for i=1,#sorted_edges do
        local e = sorted_edges[i]
        local color

        if e.face == nil then
            if e.vtx.edge == e then
                color = "yellow"
            else
                color = "green"
            end
        else
            if e.vtx.edge == e then
                color = "blue"
            else
                color = nil
            end
        end

        if color ~= nil then
            out:write("v",conv(e.vtx.id)," -> v",conv(e.next.vtx.id)," [color="..color.."]\n")
        else
            out:write("v",conv(e.vtx.id)," -> v",conv(e.next.vtx.id),"\n")
        end
    end

    out:write("}\n")
end

function Mesh:check()
    for _,f in pairs(self.faces) do
        f:check()
    end
end

-- TESTSUITE ------------------------------------------------------

function test(c, out)
    mesh = Mesh:new()

    if c == "case1" then
        local f = mesh:add_face(1,2,3)
        assert(f[1].vtx.id == 1, f[1].vtx.id.." != 1")
        assert(f[2].vtx.id == 2, f[2].vtx.id.." != 2")
        assert(f[3].vtx.id == 3, f[3].vtx.id.." != 3")
        assert(f[4] == nil)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 6, "wrong number of edges: "..nedges)
        assert(nvertices == 3, "wrong number of vertices: "..nvertices)
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
    elseif c == "case2" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,3)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
    elseif c == "case3" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,5)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 12, "wrong number of edges: "..nedges)
        assert(nvertices == 5, "wrong number of vertices: "..nvertices)
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
    elseif c == "case4" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,5)
        mesh:add_face(2,5,3)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 14, "wrong number of edges: "..nedges)
        assert(nvertices == 5, "wrong number of vertices: "..nvertices)
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "case5" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,5)
        mesh:add_face(3,5,6)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 18, "wrong number of edges: "..nedges)
        assert(nvertices == 6, "wrong number of vertices: "..nvertices)
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "case6" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,5)
        mesh:add_face(3,5,6)
        mesh:add_face(3,2,5)
        local nfaces,nvertices,nedges
            = mesh:face_count(),mesh:vertex_count(),mesh:edge_count()
        assert(nedges == 18, "wrong number of edges: "..nedges)
        assert(nvertices == 6, "wrong number of vertices: "..nvertices)
        assert(nfaces == 4, "wrong number of faces: "..nfaces)
    elseif c == "case7" then
        mesh:add_face(1,2,3)
        mesh:add_face(2,4,5)
        mesh:add_face(3,5,6)
        mesh:add_face(5,3,2)
        mesh:add_face(10,7,1)
        mesh:add_face(7,8,2)
        mesh:add_face(8,9,4)
        mesh:add_face(2,1,7)
        mesh:add_face(8,4,2)
    elseif c == "case8" then
        mesh:add_face(3,1,2)
        mesh:add_face(1,4,2)
        mesh:add_face(4,5,2)
        mesh:add_face(5,6,2)
        mesh:add_face(6,7,2)
        mesh:add_face(7,3,2)
    elseif c == 'triangulate3' then
        local f = mesh:add_face(1,2,3)
        -- should be identity
        local e = f.edge
        mesh:triangulate(e)
        assert(f.edge == e)
        assert(e.face == f)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 6, "wrong number of edges: "..nedges)
        assert(nvertices == 3, "wrong number of vertices: "..nvertices)
    elseif c == 'triangulate4' then
        local f = mesh:add_face(1,2,3,4)
        local e = f.edge
        mesh:triangulate(e)
        assert(f.edge == e)
        assert(e.face == f)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c == 'triangulate5' then
        local f = mesh:add_face(1,2,3,4,5)
        local e = f.edge
        mesh:triangulate(e)
        assert(f.edge == e)
        assert(e.face == f)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
        assert(nedges == 14, "wrong number of edges: "..nedges)
        assert(nvertices == 5, "wrong number of vertices: "..nvertices)
    elseif c == 'triangulate5_newface' then
        local f = mesh:add_face(1,2,3,4,5)
        local e = f.edge
        mesh:triangulate(e,false)
        assert(e.face ~= f)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
        assert(nedges == 14, "wrong number of edges: "..nedges)
        assert(nvertices == 5, "wrong number of vertices: "..nvertices)

    elseif c == "add_edge_prev_next" then
        local vtx = mesh:add_face(1,2,3,4)
        local face = vtx.edge.face
        local e = mesh:add_edge(1,3,vtx.edge.prev,vtx.edge.prev.prev)
        e:check()
        assert(e.face == face)

        assert(e.face == face)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)

    elseif c == "add_edge_prev_next_newfaces" then
        local vtx = mesh:add_face(1,2,3,4)
        local face = vtx.edge.face
        local e = mesh:add_edge(1,3,vtx.edge.prev,vtx.edge.prev.prev, false)
        e:check()
        assert(e.face ~= face)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)

    elseif c == "add_edge_prev" then
        local vtx = mesh:add_face(1,2,3,4)
        local face = vtx.edge.face
        local e = mesh:add_edge(1,3,vtx.edge.prev)
        e:check()
        assert(e.face == face)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)

    elseif c == "add_edge_prev_newfaces" then
        local vtx = mesh:add_face(1,2,3,4)
        local face = vtx.edge.face
        local e = mesh:add_edge(1,3,vtx.edge.prev, false)
        e:check()
        assert(e.face ~= face)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c == "add_disconnected_edge" then
        local e = mesh:add_edge(1,2)
        e:check()

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 0, "wrong number of faces: "..nfaces)
        assert(nedges == 2, "wrong number of edges: "..nedges)
        assert(nvertices == 2, "wrong number of vertices: "..nvertices)
    elseif c == "add_semi_connected_edge" then
        local f = mesh:add_face(1,2,3)
        local e = mesh:add_edge(2, 4, f.edge)

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 8, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c == "split_face" then
        local f = mesh:add_face(1,2,3)
        local vtx = mesh:split_face(f,4)
        assert(vtx.edge.face == f)
        local nfaces = mesh:face_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "split_face_new_faces" then
        local f = mesh:add_face(1,2,3)
        local vtx = mesh:split_face(f,4, false)
        for e in vtx:out_edges() do
            assert(e.face ~= f)
        end
        local nfaces = mesh:face_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "split_edge3" then
        local f = mesh:add_face(1,2,3)
        local vtx = mesh:split_edge(f.edge,4)
        local nfaces = mesh:face_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
    elseif c == "split_edge4" then
        local f = mesh:add_face(1,2,3,4)
        local fe = f.edge
        local vtx = mesh:split_edge(f.edge,5)
        assert(f.edge == fe)
        assert(vtx.edge == f.edge)
        local nfaces = mesh:face_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "split_edge_new_faces4" then
        local f = mesh:add_face(1,2,3,4)
        local vtx = mesh:split_edge(f.edge,5,false)
        for e in vtx:out_edges() do
            assert(e.face ~= f)
        end
        local nfaces = mesh:face_count()
        assert(nfaces == 3, "wrong number of faces: "..nfaces)
    elseif c == "split_edge_mid3" then
        mesh:add_face(1,2,3)
        local f = mesh:add_face(3,2,4)
        mesh:split_edge(f.edge,5)
        local nfaces = mesh:face_count()
        assert(nfaces == 4, "wrong number of faces: "..nfaces)
    elseif c == "split_edge_mid4" then
        mesh:add_face(1,2,3,4)
        local f = mesh:add_face(2,1,5,6)
        mesh:split_edge(f.edge,7)
        local nfaces = mesh:face_count()
        assert(nfaces == 6, "wrong number of faces: "..nfaces)
    elseif c == "flip_edge4" then
        mesh:add_face(1,2,3)
        local f = mesh:add_face(3,2,4)
        mesh:flip_edge(f.edge,5)
    elseif c == "flip_edge5" then
        mesh:add_face(1,2,3,4)
        local f = mesh:add_face(2,1,5,6)
        mesh:flip_edge(f.edge,7)
    elseif c == "double_flip_edge" then
        mesh:add_face(1,2,3)
        local f = mesh:add_face(3,2,4)
        mesh:flip_edge(f.edge,5)
        mesh:flip_edge(f.edge,5)
    elseif c == "flip_border" then
        local f = mesh:add_face(1,2,3)
        local did = mesh:flip_edge(f.edge,1)
        assert(not did)
    elseif c== "remove_edge" then
        local f = mesh:add_face(1,2,3)
        mesh:add_face(2,1,4)
        mesh:remove_edge(f.edge)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 8, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c== "remove_border_edge" then
        local f = mesh:add_face(1,2,3)
        mesh:add_face(2,1,4)
        mesh:remove_edge(f.edge.prev)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 8, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c== "remove_disconnected_edge" then
        local e = mesh:add_edge(1,2)
        mesh:remove_edge(e)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 0, "wrong number of faces: "..nfaces)
        assert(nedges == 0, "wrong number of edges: "..nedges)
        assert(nvertices == 0, "wrong number of vertices: "..nvertices)
    elseif c== "remove_semi_connected_edge" then
        local f = mesh:add_face(1,2,3)
        local e = mesh:add_edge(2, 4, f.edge)
        mesh:remove_edge(e)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 6, "wrong number of edges: "..nedges)
        assert(nvertices == 3, "wrong number of vertices: "..nvertices)

    elseif c == "remove_inner_vertex" then
        local f = mesh:add_face(1,2,3,4)
        local vtx = mesh:split_face(f, 5)
        local did = mesh:remove_vertex(vtx)
        assert(did)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        --assert(nfaces == 1, "wrong number of faces: "..nfaces)
        --assert(nedges == 8, "wrong number of edges: "..nedges)
        --assert(nvertices == 4, "wrong number of vertices: "..nvertices)
    elseif c == "remove_border_vertex1" then
        mesh:add_face(1,2,3)
        local f = mesh:add_face(3,2,4)
        local did = mesh:remove_vertex(4)
        assert(did)

        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 1, "wrong number of faces: "..nfaces)
        assert(nedges == 6, "wrong number of edges: "..nedges)
        assert(nvertices == 3, "wrong number of vertices: "..nvertices)
    elseif c == "remove_border_vertex2" then
        mesh:add_face(4,-2,-1)
        mesh:split_face(mesh.faces[1], 1)
        mesh:split_face(mesh.faces[3], 2)
        mesh:split_face(mesh.faces[1], 3)

        mesh:check()

        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 7, "wrong number of faces: "..nfaces)
        assert(nedges == 24, "wrong number of edges: "..nedges)
        assert(nvertices == 6, "wrong number of vertices: "..nvertices)

        mesh:remove_vertex(-1)
        local nfaces = mesh:face_count()
        local nedges = mesh:edge_count()
        local nvertices = mesh:vertex_count()
        assert(nfaces == 4, "wrong number of faces: "..nfaces)
        assert(nedges == 16, "wrong number of edges: "..nedges)
        assert(nvertices == 5, "wrong number of vertices: "..nvertices)

        mesh:check()

        mesh:remove_vertex(-2)
        nfaces = mesh:face_count()
        nedges = mesh:edge_count()
        nvertices = mesh:vertex_count()
        assert(nfaces == 2, "wrong number of faces: "..nfaces)
        assert(nedges == 10, "wrong number of edges: "..nedges)
        assert(nvertices == 4, "wrong number of vertices: "..nvertices)

        mesh:check()
    end

    mesh:check()

    mesh:output_dot(out)
end

-- BENCHMARKING ------------------------------------------------------

local function stats(clk, mesh)
    print("Faces: ",#mesh.faces)
    print("Vertices: ",#mesh.vertices)
    print("Elapsed: ",os.clock()-clk)
    print()
end

-- isolated triangles
function bench1(N)
    N = N or 10000

    print("Isolated triangles",N)

    N = N*3 -- 3 vertices per triangle

    local clk = os.clock()

    local mesh = Mesh:new()

    for i=1,N,3 do
        mesh:add_face(i,i+1,i+2)
    end

    stats(clk, mesh)

    mesh:check()
end

-- triangle strip
function bench2(N)
    N = N or 10000

    print("Triangle strip",N)

    N = N+2

    local clk = os.clock()

    local mesh = Mesh:new()

    mesh:add_face(1,3,2)

    for i=4,N do
        if i % 2 == 0 then
            mesh:add_face(i-2,i-1,i)
        else
            mesh:add_face(i-1,i-2,i)
        end
    end

    stats(clk, mesh)
    mesh:check()

--    mesh:output_dot(io.open("saida.dot","w"))
end

-- triangle fan
function bench3(N)
    N = N or 10000
    print("Triangle fan",N)
    N = N+2

    local clk = os.clock()

    local mesh = Mesh:new()

    mesh:add_face(1,2,3)

    for i=4,N do
        mesh:add_face(1,i-1,i)
    end

    stats(clk, mesh)
    mesh:check()
end

-- face split
function bench4(N)
    N = N or 10000
    print("Face split",N)
    N = N/2+2

    local clk = os.clock()

    local mesh = Mesh:new()

    local face = mesh:add_face(1,2,3)

    for i=4,N do
        mesh:split_face(face,i)
    end

    stats(clk, mesh)
    mesh:check()
end

-- edge split
function bench5(N)
    N = N or 10000
    print("Edge split",N)
    N = N/2+3

    local clk = os.clock()

    local mesh = Mesh:new()

    mesh:add_face(1,2,3)
    local face = mesh:add_face(3,2,4)

    for i=5,N do
        mesh:split_edge(face.edge,i)
    end

    stats(clk, mesh)
    mesh:check()
end

-- edge flip
function bench6(N)
    N = N or 10000
    print("Edge flip",N)

    local clk = os.clock()

    local mesh = Mesh:new()

    mesh:add_face(1,2,3)
    local face = mesh:add_face(3,2,4)

    for i=1,N do
        mesh:flip_edge(face.edge,i)
    end

    stats(clk, mesh)
    mesh:check()
end

-- remove edge
function bench7(N)
    N = N or 10000
    print("Remove edge",N)

    local mesh = Mesh:new()

    local face = mesh:add_face(1,2,3)

    for i=4,N do
        mesh:split_face(face,i)
    end

    local edges = {}
    for _,e in pairs(mesh.edges) do
        edges[#edges+1] = e
    end

    local clk = os.clock()

    for i=1,#edges do
        mesh:remove_edge(edges[i])
    end

    stats(clk, mesh)
    mesh:output_dot(io.open("saida.dot","w"))
    mesh:check()
end

function bench_all(N)
    bench1(N)
    bench2(N)
    bench3(N)
    bench4(N)
    bench5(N)
    bench6(N)
    bench7(N)
end

return M
