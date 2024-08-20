-- Licensed under GPLv2
--if not ffi then error("Requires FFI library on LuaJIT, enable jit_ffi_enable in config/global.json") end
local bit32 = bit32
local RISCV = {reg = {}, pc = 0, syscalls = {}, opcodes = {[0x63] = {}, [0x03] = {}, [0x23] = {}, [0x13] = {}, [0x33] = {}}, mult_opcodes = {}, halt = false}
for i = 1, 31 do RISCV.reg[i] = 0 end
setmetatable(RISCV.reg, {__index = function() return 0 end, __newindex = function() end})
local fficopy, ffistring, canDrawFFI
if ffi then
    RISCV.mem = ffi.new("uint8_t[?]", 0x2010000)
    RISCV.mem16 = ffi.cast("uint16_t*", RISCV.mem)
    RISCV.mem32 = ffi.cast("uint32_t*", RISCV.mem)
    fficopy, ffistring = ffi.copy, ffi.string
    canDrawFFI = pcall(term.drawPixels, 0, 0, RISCV.mem, 1, 1)
else
    local function __add(self, offset)
        return setmetatable({}, {
            __index = function(_, idx) return self[offset+idx] end,
            __newindex = function(_, idx, val) self[offset+idx] = val end,
            __add = __add
        })
    end
    RISCV.mem = setmetatable({}, {
        __index = function() return 0 end,
        __add = __add
    })
    RISCV.mem16 = setmetatable({}, {
        __index = function(_, idx) return RISCV.mem[idx*2] + RISCV.mem[idx*2+1] * 256 end,
        __newindex = function(_, idx, val)
            RISCV.mem[idx*2] = bit32.extract(val, 0, 8)
            RISCV.mem[idx*2+1] = bit32.extract(val, 8, 8)
        end,
        __add = __add
    })
    RISCV.mem32 = setmetatable({}, {
        __index = function(_, idx) return RISCV.mem[idx*4] + RISCV.mem[idx*4+1] * 256 + RISCV.mem[idx*4+2] * 65536 + RISCV.mem[idx*4+3] * 16777216 end,
        __newindex = function(_, idx, val)
            RISCV.mem[idx*4] = bit32.extract(val, 0, 8)
            RISCV.mem[idx*4+1] = bit32.extract(val, 8, 8)
            RISCV.mem[idx*4+2] = bit32.extract(val, 16, 8)
            RISCV.mem[idx*4+3] = bit32.extract(val, 24, 8)
        end,
        __add = __add
    })
    function fficopy(dest, src, size)
        if type(src) == "string" then
            for i, c in src:gmatch "()(.)" do
                dest[i-1] = c:byte()
            end
        else
            for i = 0, size - 1 do
                dest[i] = src[i]
            end
        end
    end
    function ffistring(ptr, size)
        local retval = ""
        if size then
            for i = 0, size - 1 do retval = retval .. string.char(ptr[i]) end
        else
            for i = 0, math.huge do
                local c = ptr[i]
                if c == 0 then break end
                retval = retval .. string.char(c)
            end
        end
        return retval
    end
    canDrawFFI = false
end

local seek_types = {[0] = "set", "cur", "end"}

local keymap = {
    [keys.a] = 97,
    [keys.b] = 98,
    [keys.c] = 99,
    [keys.d] = 100,
    [keys.e] = 101,
    [keys.f] = 102,
    [keys.g] = 103,
    [keys.h] = 104,
    [keys.i] = 105,
    [keys.j] = 106,
    [keys.k] = 107,
    [keys.l] = 108,
    [keys.m] = 109,
    [keys.n] = 110,
    [keys.o] = 111,
    [keys.p] = 112,
    [keys.q] = 113,
    [keys.r] = 114,
    [keys.s] = 115,
    [keys.t] = 116,
    [keys.u] = 117,
    [keys.v] = 118,
    [keys.w] = 119,
    [keys.x] = 120,
    [keys.y] = 121,
    [keys.z] = 122,
    [keys.zero] = 48,
    [keys.one] = 48,
    [keys.two] = 50,
    [keys.three] = 51,
    [keys.four] = 52,
    [keys.five] = 53,
    [keys.six] = 54,
    [keys.seven] = 55,
    [keys.eight] = 56,
    [keys.nine] = 57,
    [keys.right] = 0xae,
    [keys.left] = 0xac,
    [keys.up] = 0xad,
    [keys.down] = 0xaf,
    [keys.grave] = 27,
    [keys.enter] = 13,
    [keys.tab] = 9,
    [keys.backspace] = 127,
    [keys.rightShift] = 0x80+0x36,
    [keys.rightCtrl] = 0x80+0x1d,
    [keys.rightAlt] = 0x80+0x38,
    [keys.equals] = 0x3d,
    [keys.minus] = 0x2d,
    [keys.f1] = 0x80+0x3b,
    [keys.f2] = 0x80+0x3c,
    [keys.f3] = 0x80+0x3d,
    [keys.f4] = 0x80+0x3e,
    [keys.f5] = 0x80+0x3f,
    [keys.f6] = 0x80+0x40,
    [keys.f7] = 0x80+0x41,
    [keys.f8] = 0x80+0x42,
    [keys.f9] = 0x80+0x43,
    [keys.f10] = 0x80+0x44,
    [keys.f11] = 0x80+0x57,
    [keys.f12] = 0x80+0x58,
    [keys.space] = 32,
}

local speaker = peripheral.find "speaker"
local files = {
    [0] = io.stdin,
    io.stdout,
    io.stderr,
    { -- dsp
        write = function(self, data)
            if speaker then
                local samples = {}
                for i, m in data:gmatch "()(....)" do
                    local n = math.floor(("<h"):unpack(m) / 256)
                    samples[i] = n
                    samples[i+1] = n
                    samples[i+2] = n
                    samples[i+3] = n
                end
                speaker.playAudio(samples)
            end
        end,
        close = function() end
    }
}

RISCV.syscalls[0] = function(self, code) -- exit
    --error("fail")
    self.halt = true
    return 0
end

RISCV.syscalls[1] = function(self, _path, flags) -- open
    local path = ffistring(self.mem + _path)
    if path == "/dev/dsp" then return 3 end
    if bit32.btest(bit32.band(flags, 3), 2) then return -1 end
    local mode = bit32.btest(flags, 1) and (bit32.btest(flags, 8) and "ab" or "wb") or "rb"
    local file, err = io.open(path, mode)
    if file then
        local fp = #files+1
        files[fp] = file
        return fp
    else
        --io.stderr:write(err)
        return 0xFFFFFFFF
    end
end

RISCV.syscalls[2] = function(self, fp, _buf, len) -- read
    local buf = self.mem + _buf
    local file = files[fp]
    if not file then return 0 end
    local data = file:read(len)
    if not data then return 0 end
    fficopy(buf, data)
    return #data
end

RISCV.syscalls[3] = function(self, fp, _buf, len) -- write
    local buf = ffistring(self.mem + _buf, len)
    local file = files[fp]
    if not file then return 0 end
    file:write(buf)
    return #buf
end

RISCV.syscalls[4] = function(self, fp) -- close
    local file = files[fp]
    if not file then return -1 end
    file:close()
    files[fp] = nil
    return 0
end

RISCV.syscalls[5] = function(self, _path, _st) -- stat
    local path = ffistring(self.mem + _path)
    --local st = ffi.cast()
    return 0xFFFFFFFF
end

RISCV.syscalls[6] = function(self, _us) -- epoch
    local time = os.epoch("utc")
    if _us ~= 0 then self.mem32[_us / 4] = time % 1000 end
    return math.floor(time / 1000)
end

RISCV.syscalls[7] = function(self, _path, mode) -- access
    local path = ffistring(self.mem + _path)
    local exists = fs.exists(path)
    local ro = fs.isReadOnly(path)
    local res = (exists and not (bit32.btest(mode, 2) and ro)) and 0 or 0xFFFFFFFF
    --print("access " .. path .. " (" .. mode .. "): " .. res)
    return res
end

RISCV.syscalls[8] = function(self, us) -- usleep
    sleep(us / 1000000)
    return 0
end

RISCV.syscalls[9] = function(self, fp, offset, whence) -- seek
    local file = files[fp]
    if not file then return 0 end
    return file:seek(seek_types[whence], offset) or 0xFFFFFFFF
end

RISCV.syscalls[10] = function(self, mode) -- setGraphicsMode
    term.setGraphicsMode(mode)
    if mode == 2 then term.clear() end
    return 0
end

RISCV.syscalls[11] = function(self) -- updatePalette
    for i = 0, 255 do
        term.setPaletteColor(i,
            self.mem[0x200FA00 + i*3] / 255,
            self.mem[0x200FA00 + i*3 + 1] / 255,
            self.mem[0x200FA00 + i*3 + 2] / 255
        )
    end
    return 0
end

RISCV.syscalls[12] = function(self) -- updateScreen
    if canDrawFFI then
        term.drawPixels(0, 0, self.mem + 0x2000000, 320, 200)
    else
        local screen = {}
        for y = 1, 200 do screen[y] = ffistring(self.mem + 0x2000000 + (y-1)*320, 320) end
        term.drawPixels(0, 0, screen)
    end
    return 0
end

local eventQueue = {}
RISCV.syscalls[13] = function(self, _ev) -- getEvent
    while #eventQueue > 0 do
        local ev = table.remove(eventQueue, 1)
        if ev[1] == "key" then
            self.mem[_ev] = 0
            self.mem[_ev+1] = keymap[ev[2]] or ev[2]
            return 1
        elseif ev[1] == "key_up" then
            self.mem[_ev] = 1
            self.mem[_ev+1] = keymap[ev[2]] or ev[2]
            return 1
        elseif ev[1] == "mouse_down" then
            self.mem[_ev] = 2
            self.mem[_ev+1] = ev[2]
            self.mem16[_ev / 2 + 1] = ev[3]
            self.mem16[_ev / 2 + 2] = ev[4]
            return 1
        elseif ev[1] == "mouse_up" then
            self.mem[_ev] = 3
            self.mem[_ev+1] = ev[2]
            self.mem16[_ev / 2 + 1] = ev[3]
            self.mem16[_ev / 2 + 2] = ev[4]
            return 1
        elseif ev[1] == "mouse_drag" or ev[1] == "mouse_move" then
            self.mem[_ev] = 4
            self.mem[_ev+1] = 0
            self.mem16[_ev / 2 + 1] = ev[3] or 0
            self.mem16[_ev / 2 + 2] = ev[4] or 0
            return 1
        end
    end
    return 0
end

local function signed(num)
    if bit32.btest(num, 0x80000000) then return num - 0x100000000 end
    return num
end

local function fiximm(bits) return function(inst)
    if bit32.btest(inst.imm, 2^(bits-1)) then inst.simm = bit32.bor(inst.imm, bit32.bnot(2^bits-1)) - 0x100000000
    else inst.simm = inst.imm end
    return inst
end end

local function decodeR(inst)
    return {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        funct7 = bit32.extract(inst, 25, 7)
    }
end

local function decodeI(inst)
    return fiximm(12) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        imm = bit32.extract(inst, 20, 12)
    }
end

local function decodeS(inst)
    return fiximm(12) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        imm = bit32.bor(bit32.extract(inst, 7, 5), bit32.lshift(bit32.extract(inst, 25, 7), 5))
    }
end

local function decodeB(inst)
    return fiximm(13) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        funct3 = bit32.extract(inst, 12, 3),
        rs1 = bit32.extract(inst, 15, 5),
        rs2 = bit32.extract(inst, 20, 5),
        imm = bit32.bor(
            bit32.lshift(bit32.extract(inst, 7, 1), 11),
            bit32.lshift(bit32.extract(inst, 8, 4), 1),
            bit32.lshift(bit32.extract(inst, 25, 6), 5),
            bit32.lshift(bit32.extract(inst, 31, 1), 12)
        )
    }
end

local function decodeU(inst)
    return fiximm(32) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        imm = bit32.band(inst, 0xFFFFF000)
    }
end

local function decodeJ(inst)
    return fiximm(21) {
        inst = inst,
        opcode = bit32.extract(inst, 0, 7),
        rd = bit32.extract(inst, 7, 5),
        imm = bit32.bor(
            bit32.lshift(bit32.extract(inst, 12, 8), 12),
            bit32.lshift(bit32.extract(inst, 20, 1), 11),
            bit32.lshift(bit32.extract(inst, 21, 10), 1),
            bit32.lshift(bit32.extract(inst, 31, 1), 20)
        )
    }
end

local opcode_modes = {
    [0x37] = decodeU, -- LUI
    [0x17] = decodeU, -- AUIPC
    [0x6F] = decodeJ, -- JAL
    [0x67] = decodeI, -- JALR
    [0x63] = decodeB, -- B*
    [0x03] = decodeI, -- L*
    [0x23] = decodeS, -- S*
    [0x13] = decodeI, -- immediate arith
    [0x33] = decodeR, -- arith
    [0x0F] = decodeI, -- FENCE
    [0x73] = decodeI, -- E*
}

RISCV.opcodes[0x37] = function(self, inst) -- LUI
    self.reg[inst.rd] = inst.imm
    return "LUI x" .. inst.rd .. ", " .. inst.imm
end

RISCV.opcodes[0x17] = function(self, inst) -- AUIPC
    self.reg[inst.rd] = (self.pc - 4 + inst.imm) % 0x100000000
    return "AUIPC x" .. inst.rd .. ", " .. inst.imm
end

RISCV.opcodes[0x6F] = function(self, inst) -- JAL
    self.reg[inst.rd] = self.pc
    self.pc = self.pc + inst.simm - 4
    if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    return "JAL x" .. inst.rd .. ", " .. inst.simm
end

RISCV.opcodes[0x67] = function(self, inst) -- JALR
    local oldpc = self.pc
    self.pc = bit32.band(self.reg[inst.rs1] + inst.simm, 0xFFFFFFFE)
    self.reg[inst.rd] = oldpc
    if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    return "JALR x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][0] = function(self, inst) -- BEQ
    if self.reg[inst.rs1] == self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BEQ x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][1] = function(self, inst) -- BNE
    if self.reg[inst.rs1] ~= self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BNE x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][4] = function(self, inst) -- BLT
    if signed(self.reg[inst.rs1]) < signed(self.reg[inst.rs2]) then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BLT x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][5] = function(self, inst) -- BGE
    if signed(self.reg[inst.rs1]) >= signed(self.reg[inst.rs2]) then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BGE x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][6] = function(self, inst) -- BLTU
    if self.reg[inst.rs1] < self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BLTU x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x63][7] = function(self, inst) -- BGEU
    if self.reg[inst.rs1] >= self.reg[inst.rs2] then
        self.pc = self.pc + inst.simm - 4
        if self.pc % 4 ~= 0 then error("unaligned jump to " .. self.pc) end
    end
    return "BGEU x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][0] = function(self, inst) -- LB
    self.reg[inst.rd] = self.mem[self.reg[inst.rs1] + inst.simm]
    if bit32.btest(self.reg[inst.rd], 0x80) then self.reg[inst.rd] = bit32.bor(self.reg[inst.rd], 0xFFFFFF00) end
    return "LB x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][1] = function(self, inst) -- LH
    local addr = self.reg[inst.rs1] + inst.simm
    if addr % 2 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256
    else self.reg[inst.rd] = self.mem16[addr / 2] end
    if bit32.btest(self.reg[inst.rd], 0x8000) then self.reg[inst.rd] = bit32.bor(self.reg[inst.rd], 0xFFFF0000) end
    return "LH x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][2] = function(self, inst) -- LW
    local addr = self.reg[inst.rs1] + inst.simm
    if addr % 4 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256 + self.mem[addr+2] * 65536 + self.mem[addr+3] * 16777216
    else self.reg[inst.rd] = self.mem32[addr / 4] end
    return "LW x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][4] = function(self, inst) -- LBU
    self.reg[inst.rd] = self.mem[self.reg[inst.rs1] + inst.simm]
    return "LBU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x03][5] = function(self, inst) -- LHU
    local addr = self.reg[inst.rs1] + inst.simm
    if addr % 2 ~= 0 then self.reg[inst.rd] = self.mem[addr] + self.mem[addr+1] * 256
    else self.reg[inst.rd] = self.mem16[addr / 2] end
    return "LHU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][0] = function(self, inst) -- SB
    self.mem[self.reg[inst.rs1] + inst.simm] = bit32.band(self.reg[inst.rs2], 0xFF)
    return "SB x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][1] = function(self, inst) -- SH
    local addr = self.reg[inst.rs1] + inst.simm
    if addr % 2 ~= 0 then
        self.mem[addr] = bit32.extract(self.reg[inst.rs2], 0, 8)
        self.mem[addr+1] = bit32.extract(self.reg[inst.rs2], 8, 8)
    else self.mem16[addr / 2] = bit32.band(self.reg[inst.rs2], 0xFFFF) end
    return "SH x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x23][2] = function(self, inst) -- SW
    local addr = self.reg[inst.rs1] + inst.simm
    if addr % 4 ~= 0 then
        self.mem[addr] = bit32.extract(self.reg[inst.rs2], 0, 8)
        self.mem[addr+1] = bit32.extract(self.reg[inst.rs2], 8, 8)
        self.mem[addr+2] = bit32.extract(self.reg[inst.rs2], 16, 8)
        self.mem[addr+3] = bit32.extract(self.reg[inst.rs2], 24, 8)
    else self.mem32[addr / 4] = self.reg[inst.rs2] end
    return "SW x" .. inst.rs1 .. ", x" .. inst.rs2 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][0] = function(self, inst) -- ADDI
    self.reg[inst.rd] = (self.reg[inst.rs1] + inst.simm) % 0x100000000
    return "ADDI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][2] = function(self, inst) -- SLTI
    self.reg[inst.rd] = signed(self.reg[inst.rs1]) < inst.simm and 1 or 0
    return "SLTI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][3] = function(self, inst) -- SLTIU
    local imm = inst.simm
    if imm < 0 then imm = imm + 0x100000000 end
    self.reg[inst.rd] = self.reg[inst.rs1] < imm and 1 or 0
    return "SLTIU x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][4] = function(self, inst) -- XORI
    self.reg[inst.rd] = bit32.bxor(self.reg[inst.rs1], inst.simm)
    return "XORI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][6] = function(self, inst) -- ORI
    self.reg[inst.rd] = bit32.bor(self.reg[inst.rs1], inst.simm)
    return "ORI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][7] = function(self, inst) -- ANDI
    self.reg[inst.rd] = bit32.band(self.reg[inst.rs1], inst.simm)
    return "ANDI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.simm
end

RISCV.opcodes[0x13][1] = function(self, inst) -- SLLI
    self.reg[inst.rd] = bit32.lshift(self.reg[inst.rs1], bit32.band(inst.imm, 0x1F))
    return "SLLI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x13][5] = function(self, inst) -- SRLI/SRAI
    self.reg[inst.rd] = (bit32.btest(inst.imm, 0x400) and bit32.arshift or bit32.rshift)(self.reg[inst.rs1], bit32.band(inst.imm, 0x1F))
    return "SRLI/SRAI x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.imm
end

RISCV.opcodes[0x33][0] = function(self, inst) -- ADD/SUB
    if bit32.btest(inst.funct7, 0x20) then self.reg[inst.rd] = (self.reg[inst.rs1] - self.reg[inst.rs2]) % 0x100000000
    else self.reg[inst.rd] = (self.reg[inst.rs1] + self.reg[inst.rs2]) % 0x100000000 end
    return "ADD/SUB x" .. inst.rd .. ", x" .. inst.rs1 .. ", " .. inst.rs2
end

RISCV.opcodes[0x33][1] = function(self, inst) -- SLL
    self.reg[inst.rd] = bit32.lshift(self.reg[inst.rs1], bit32.band(self.reg[inst.rs2], 0x1F))
    return "SLL x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][2] = function(self, inst) -- SLT
    self.reg[inst.rd] = signed(self.reg[inst.rs1]) < signed(self.reg[inst.rs2]) and 1 or 0
    return "SLT x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][3] = function(self, inst) -- SLTU
    self.reg[inst.rd] = self.reg[inst.rs1] < self.reg[inst.rs2] and 1 or 0
    return "SLTU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][4] = function(self, inst) -- XOR
    self.reg[inst.rd] = bit32.bxor(self.reg[inst.rs1], self.reg[inst.rs2])
    return "XOR x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][5] = function(self, inst) -- SRL/SRA
    self.reg[inst.rd] = (bit32.btest(inst.funct7, 0x20) and bit32.arshift or bit32.rshift)(self.reg[inst.rs1], bit32.band(self.reg[inst.rs2], 0x1F))
    return "SRL/SRA x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][6] = function(self, inst) -- OR
    self.reg[inst.rd] = bit32.bor(self.reg[inst.rs1], self.reg[inst.rs2])
    return "OR x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x33][7] = function(self, inst) -- AND
    self.reg[inst.rd] = bit32.band(self.reg[inst.rs1], self.reg[inst.rs2])
    return "AND x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.opcodes[0x0F] = function(self, inst) -- FENCE
    -- do nothing
    return "FENCE"
end

RISCV.opcodes[0x73] = function(self, inst) -- ECALL/EBREAK
    if self.syscalls[self.reg[17]] then self.reg[10] = self.syscalls[self.reg[17]](self, table.unpack(self.reg, 10, 16)) end
    --if self.reg[17] == 93 then self.halt = true end
    --print("ECALL " .. self.reg[17])
    return "ECALL " .. self.reg[17]
end

RISCV.mult_opcodes[0] = function(self, inst) -- MUL
    self.reg[inst.rd] = math.abs((signed(self.reg[inst.rs1]) * signed(self.reg[inst.rs2])) % 0x100000000)
    return "MUL x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[3] = function(self, inst) -- MULHU
    self.reg[inst.rd] = math.floor((self.reg[inst.rs1] * self.reg[inst.rs2]) / 0x100000000)
    return "MULH x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[2] = function(self, inst) -- MULHSU
    self.reg[inst.rd] = math.floor((signed(self.reg[inst.rs1]) * self.reg[inst.rs2]) / 0x100000000)
    if self.reg[inst.rd] < 0 then self.reg[inst.rd] = self.reg[inst.rd] + 0x100000000 end
    return "MULHSU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[1] = function(self, inst) -- MULH
    self.reg[inst.rd] = math.floor((signed(self.reg[inst.rs1]) * signed(self.reg[inst.rs2])) / 0x100000000)
    if self.reg[inst.rd] < 0 then self.reg[inst.rd] = self.reg[inst.rd] + 0x100000000 end
    return "MULHU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[4] = function(self, inst) -- DIV
    if self.reg[inst.rs2] == 0 then
        self.reg[inst.rd] = 0xFFFFFFFF
    else
        local res = signed(self.reg[inst.rs1]) / signed(self.reg[inst.rs2])
        if res < 0 then self.reg[inst.rd] = math.ceil(res) + 0x100000000
        else self.reg[inst.rd] = math.floor(res) end
    end
    return "DIV x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[5] = function(self, inst) -- DIVU
    if self.reg[inst.rs2] == 0 then self.reg[inst.rd] = 0xFFFFFFFF
    else self.reg[inst.rd] = math.floor(self.reg[inst.rs1] / self.reg[inst.rs2]) end
    return "DIVU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[6] = function(self, inst) -- REM
    if self.reg[inst.rs2] == 0 then
        self.reg[inst.rd] = self.reg[inst.rs1]
    else
        local res = math.fmod(signed(self.reg[inst.rs1]), signed(self.reg[inst.rs2]))
        if res < 0 then self.reg[inst.rd] = math.ceil(res) + 0x100000000
        else self.reg[inst.rd] = math.floor(res) end
    end
    return "REM x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

RISCV.mult_opcodes[7] = function(self, inst) -- REMU
    if self.reg[inst.rs2] == 0 then self.reg[inst.rd] = self.reg[inst.rs1]
    else self.reg[inst.rd] = self.reg[inst.rs1] % self.reg[inst.rs2] end
    return "REMU x" .. inst.rd .. ", x" .. inst.rs1 .. ", x" .. inst.rs2
end

function RISCV:clock()
    if self.pc >= 33554432 then error("pc out of bounds") end
    local oldpc = self.pc
    local inst = self.mem32[self.pc / 4]
    self.pc = self.pc + 4
    local mode = opcode_modes[bit32.band(inst, 0x7F)]
    if not mode then
        error(("Unknown opcode %02X"):format(bit32.band(inst, 0x7F)))
        return
    end
    inst = mode(inst)
    --print(textutils.serialize(inst))
    local f = self.opcodes[inst.opcode]
    local op
    if type(f) == "function" then op = f(self, inst)
    elseif type(f) == "table" then
        if not f[inst.funct3] then print("Unknown function " .. inst.funct3)
        elseif inst.opcode == 0x33 and bit32.btest(inst.funct7, 1) then op = self.mult_opcodes[inst.funct3](self, inst)
        else op = f[inst.funct3](self, inst) end
    else error("Unknown opcode " .. inst.opcode) end
    --if op then print(("%08x  %s"):format(oldpc, op)) end
end

function RISCV:run(cycles)
    for i = 1, cycles do if self.halt then return end self:clock() --[[sleep(0.05)]] end
end

local function loadELF(data, mem)
    local magic, b32, le, headerver, abi, type, isa, elfver, entrypoint, progheadpos, sectheadpos, flags, headsize, progheadsz, nproghead, sectheadsz, nsecthead, sectnameidx =
        ("<c4BBBBxxxxxxxxHHIIIIIHHHHHH"):unpack(data)
    if magic ~= "\x7fELF" then error("invalid ELF file") end
    if b32 ~= 1 or le ~= 1 or headerver ~= 1 or abi ~= 0 or type ~= 2 or isa ~= 0xF3 or elfver ~= 1 then error("unsupported ELF file") end
    print(("entrypoint=%08x proghead @ %08x secthead @ %08x"):format(entrypoint, progheadpos, sectheadpos))
    for i = 1, nproghead do
        local type, offset, addr, filesize, memsize, flags, align = ("<IIIxxxxIIII"):unpack(data, progheadpos + (i - 1) * progheadsz + 1)
        if type == 1 then
            print(("offset=%08x address=%08x size=%08x/%08x"):format(offset, addr, filesize, memsize))
            --assert(ffi.sizeof(mem) >= addr + filesize, "not enough space")
            assert(#data >= offset + filesize, "not enough data")
            fficopy(mem + addr, data:sub(offset + 1), filesize)
        end
    end
    return entrypoint
end

-- [[
local args = {shell.getRunningProgram(), ...}
RISCV.mem32[0x803F40] = #args
local pos = 0x200FD04 + #args * 4
for i, v in ipairs(args) do
    if pos + #v + 1 > 764 then break end
    RISCV.mem32[0x803F40 + i] = pos
    fficopy(RISCV.mem + pos, v)
    pos = pos + #v + 1
end
local file = fs.open(shell.resolve("linux/linuxxdoom"), "rb")
local elf = file.readAll()
file.close()
RISCV.pc = loadELF(elf, RISCV.mem)
RISCV.reg[2] = 0x1FF0000
local ok, err = xpcall(function()
    while true do
        --local time = os.epoch "nano"
        RISCV:run(100000)
        --print(os.epoch "nano" - time)
        if RISCV.halt then break end
        os.queueEvent("__queue_back")
        repeat eventQueue[#eventQueue+1] = {os.pullEvent()}
        until eventQueue[#eventQueue][1] == "__queue_back"
        eventQueue[#eventQueue] = nil
        --print(("%08x"):format(RISCV.pc))
    end
end, function(msg)
    msg = msg .. ("\npc=%08x\n"):format(RISCV.pc)
    for y = 0, 31, 4 do
        for x = 0, 3 do
            msg = msg .. ("x%d=%08x "):format(y+x, RISCV.reg[y+x])
        end
        msg = msg .. "\n"
    end
    return msg
end)
term.setGraphicsMode(0)
for i = 0, 15 do term.setPaletteColor(2^i, term.nativePaletteColor(2^i)) end
if not ok then printError(err) end
--[=[]]
for _, name in ipairs(fs.list("linuxdoom/tests")) do
    print(name)
    local file = fs.open("linuxdoom/tests/" .. name, "rb")
    local elf = file.readAll()
    file.close()
    RISCV.halt = false
    RISCV.pc = loadELF(elf, RISCV.mem)
    RISCV.reg[2] = 0x1FF0000
    assert(xpcall(function()
        RISCV:run(10000)
        if not bit32.btest(RISCV.reg[10], 1) then print(name, "success")
        else error(name .. " failure") end
        --sleep(1)
    end, function(msg)
        msg = msg .. ("\npc=%08x\n"):format(RISCV.pc)
        for y = 0, 31, 4 do
            for x = 0, 3 do
                msg = msg .. ("x%d=%08x "):format(y+x, RISCV.reg[y+x])
            end
            msg = msg .. "\n"
        end
        return msg
    end))
end
--]=]