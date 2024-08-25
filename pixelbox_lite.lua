--[[
    * api for easy interaction with drawing characters

    * single file implementation of GuiH pixelbox api

    * this version is very fast and ment for implementing into other apis
]]

local PIXELBOX = {}
local OBJECT = {}

local t_cat,s_char,m_floor  = table.concat,string.char,math.floor

---@class Heap A min- or max-heap which efficiently adds and removes items in order.
---@operator len:number
local Heap = {}
Heap.__mt = {__name = "Heap", __index = Heap}

--- Creates a new min-heap. (O(1))
---@return Heap heap A new Heap
function Heap:new()
    return setmetatable({}, self.__mt)
end

--- Returns whether the heap is empty. (O(1))
---@return boolean empty Whether the heap is empty
function Heap:isEmpty()
    return #self == 0
end

--- Returns the length of the heap. (O(1))
---@return number length The length of the heap
function Heap:length()
    return #self
end

--- Returns the item at the top of the heap. (O(1))
---@return any|nil top The top of the heap, or nil if the heap is empty
---@return number|nil prio The priority of the value
function Heap:front()
    return self[1]
end

--- Inserts an item into the heap. (O(1) average, O(log n) worst)
---@param val any The value to insert
---@param prio number The priority of the value
function Heap:insert(val, prio)
    local idx = #self+1
    self[idx] = {val, prio}
    while idx > 1 do
        local parent = m_floor(idx / 2)
        if self[idx][2] <= self[parent][2] then
            return
        else
            self[idx], self[parent] = self[parent], self[idx]
            idx = parent
        end
    end
end

--- Removes the top item from the heap, and returns it. (O(log n))
---@return any|nil top The item previously at the top, or nil if the heap is empty
---@return number|nil prio The priority of the value
function Heap:remove()
    local retval = self[1]
    local idx = #self
    self[1], self[idx] = self[idx], nil
    idx = 1
    local child = 2
    local value = self[1][2]
    while idx <= #self do
        if self[child] == nil then return retval[1], retval[2] end
        local max, midx = value, -1
        if self[child][2] < max then
            max, midx = self[child][2], child
        end
        local cp1 = child + 1
        if cp1 <= #self and (self[cp1][2] < max) then
            max, midx = self[cp1][2], cp1
        end
        if max == value then
            return retval[1], retval[2]
        else
            self[midx], self[idx] = self[idx], self[midx]
            idx = midx
            child = 2 * idx
        end
    end
    return retval[1], retval[2]
end

--- Increases the priority of an item in the tree.
---@param idx number The index of the item in the tree
---@param prio number The amount to adjust by
function Heap:increase(idx, prio)
    local child = idx * 2
    local valuea = self[idx]
    local value = valuea[2] + prio
    valuea[2] = value
    while idx <= #self do
        if not self[child] then return end
        local max, midx = value, -1
        if self[child][2] < max then
            max, midx = self[child][2], child
        end
        local cp1 = child + 1
        if cp1 <= #self and (self[cp1][2] < max) then
            max, midx = self[cp1][2], cp1
        end
        if max == value then
            return
        else
            self[midx], self[idx] = self[idx], self[midx]
            idx = midx
            child = 2 * idx
        end
    end
end

local to_blit = {}
for i = 0, 15 do
    to_blit[2^i] = ("%x"):format(i)
end


function PIXELBOX.RESTORE(BOX,color)
    local bc = {}

    for y=1,BOX.height*3 do
        for x=1,BOX.width*2 do
            if not bc[y] then bc[y] = {} end
            bc[y][x] = color
        end
    end

    BOX.CANVAS = bc

    BOX.distances = {
        {5,256,16,8,64,32},
        {4,16,16384,256,128},
        [4]    ={4,64,1024,256,128},
        [8]    ={4,512,2048,256,1},
        [16]   ={4,2,16384,256,1},
        [32]   ={4,8192,4096,256,1},
        [64]   ={4,4,1024,256,1},
        [128]  ={6,32768,256,1024,2048,4096,16384},
        [256]  ={6,1,128,2,512,4,8192},
        [512]  ={4,8,2048,256,128},
        [1024] ={4,4,64,128,32768},
        [2048] ={4,512,8,128,32768},
        [4096] ={4,8192,32,128,32768},
        [8192] ={3,32,4096,256128},
        [16384]={4,2,16,128,32768},
        [32768]={5,128,1024,2048,4096,16384}
    }
end

local function build_drawing_char(distances,a,b,c,d,e,f)
    local arr = {a,b,c,d,e,f}
    local c_types, c_idx = {}, {}
    local sortable = Heap:new()
    local ind = 0
    for i=1,6 do
        local c = arr[i]
        local ct = c_types[c]
        if ct then
            c_types[c] = ct + 1
        else
            ind = ind + 1
            c_types[c] = 1
            c_idx[ind] = c
        end
    end
    for i = 1, ind do
        local idx = c_idx[i]
        sortable:insert(idx, c_types[idx])
    end
    local n = #sortable
    while n > 2 do
        local col, cn = sortable:remove()
        local bit6 = distances[col]
        local index,run = 1,false
        local nm1 = n - 1
        for i=2,bit6[1] do
            if run then break end
            local tab = bit6[i]
            for j=1,nm1 do
                if sortable[j][1] == tab then
                    index = j
                    run = true
                    break
                end
            end
        end
        local from,to = col,sortable[index][1]
        for i=1,6 do
            if arr[i] == from then
                arr[i] = to
            end
        end
        sortable:increase(index, cn)
        n = n - 1
    end

    local n = 128
    local a6 = arr[6]

    if arr[1] ~= a6 then n = n + 1 end
    if arr[2] ~= a6 then n = n + 2 end
    if arr[3] ~= a6 then n = n + 4 end
    if arr[4] ~= a6 then n = n + 8 end
    if arr[5] ~= a6 then n = n + 16 end

    if sortable[1][1] == arr[6] then
        return s_char(n),sortable[2][1],arr[6]
    else
        return s_char(n),sortable[1][1],arr[6]
    end
end

function OBJECT:render()
    local t = self.term
    local blit_line,set_cursor = t.blit,t.setCursorPos

    local w_double = self.width*2
    local canv = self.CANVAS
    local distances = self.distances

    local sy = 0
    for y=1,self.height*3,3 do
        sy = sy + 1
        local layer_1 = canv[y]
        local layer_2 = canv[y+1]
        local layer_3 = canv[y+2]
        local char_line,fg_line,bg_line = {},{},{}
        local n = 0
        for x=1,w_double,2 do
            local xp1 = x+1
            local b11,b21,b12,b22,b13,b23 =
                layer_1[x],layer_1[xp1],
                layer_2[x],layer_2[xp1],
                layer_3[x],layer_3[xp1]

            local char,fg,bg = " ",1,b11
            if not (b21 == b11
                and b12 == b11
                and b22 == b11
                and b13 == b11
                and b23 == b11) then
                char,fg,bg = build_drawing_char(distances,b11,b21,b12,b22,b13,b23)
            end
            n = n + 1
            char_line[n] = char
            fg_line  [n] = to_blit[fg]
            bg_line  [n] = to_blit[bg]
        end

        set_cursor(1,sy)
        blit_line(
            t_cat(char_line,""),
            t_cat(fg_line,""),
            t_cat(bg_line,"")
        )
    end
end

function OBJECT:clear(color)
    PIXELBOX.RESTORE(self,color)
end 

function OBJECT:set_pixel(x,y,color)
    self.CANVAS[y][x] = color
end

function PIXELBOX.new(terminal,bg)
    local bg = bg or terminal.getBackgroundColor() or colors.black
    local BOX = {}
    local w,h = terminal.getSize()
    BOX.term = terminal
    setmetatable(BOX,{__index = OBJECT})
    BOX.width  = w
    BOX.height = h
    PIXELBOX.RESTORE(BOX,bg)
    return BOX
end

return PIXELBOX
