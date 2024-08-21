-- Licensed under GPLv2
--if not ffi then error("Requires FFI library on LuaJIT, enable jit_ffi_enable in config/global.json") end
local bit32 = bit32
local RISCV = {reg = {}, pc = 0, syscalls = {}, opcodes = {[0x63] = {}, [0x03] = {}, [0x23] = {}, [0x13] = {}, [0x33] = {}}, mult_opcodes = {}, halt = false}
for i = 1, 31 do RISCV.reg[i] = 0 end
setmetatable(RISCV.reg, {__index = function() return 0 end, __newindex = function() end})
local fficopy, ffistring, canDrawFFI
if ffi then
    print("Using FFI acceleration")
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
    [keys.leftCtrl] = 157,
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
    return code
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
        --[[elseif ev[1] == "mouse_down" then
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
            return 1]]
        end
    end
    return 0
end

RISCV.syscalls[14] = function(self) -- getTick
    return math.floor((os.epoch "utc" / 1000) * 70) % 0x100000000
end

-- for tests
RISCV.syscalls[93] = function(self, code)
    self.halt = true
    return code
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

RISCV.opcodes[0x37] = function(pc, inst) -- LUI
    return ([[
        self.reg[%d] = %d
    ]]):format(inst.rd, inst.imm)
end

RISCV.opcodes[0x17] = function(pc, inst) -- AUIPC
    return ([[
        self.reg[%d] = %d
    ]]):format(inst.rd, (pc - 4 + inst.imm) % 0x100000000)
end

RISCV.opcodes[0x6F] = function(pc, inst) -- JAL
    if (pc + inst.simm - 4) % 4 ~= 0 then error("unaligned jump to " .. (pc + inst.simm - 4)) end
    return ([[
        self.reg[%d] = %d
        return self.traces[%d](self)
    ]]):format(inst.rd, pc, pc + inst.simm - 4), true
end

RISCV.opcodes[0x67] = function(pc, inst) -- JALR
    return ([[
        local pc = bit32.band(self.reg[%d] + %d, 0xFFFFFFFE)
        self.reg[%d] = %d
        return self.traces[pc](self)
    ]]):format(inst.rs1, inst.simm, inst.rd, pc), true
end

RISCV.opcodes[0x63][0] = function(pc, inst) -- BEQ
    return ([[
        if self.reg[%d] == self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][1] = function(pc, inst) -- BNE
    return ([[
        if self.reg[%d] ~= self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][4] = function(pc, inst) -- BLT
    return ([[
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        if ra < rb then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][5] = function(pc, inst) -- BGE
    return ([[
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        if ra >= rb then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][6] = function(pc, inst) -- BLTU
    return ([[
        if self.reg[%d] < self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x63][7] = function(pc, inst) -- BGEU
    return ([[
        if self.reg[%d] >= self.reg[%d] then return self.traces[%d](self)
        else return self.traces[%d](self) end
    ]]):format(inst.rs1, inst.rs2, pc + inst.simm - 4, pc), true
end

RISCV.opcodes[0x03][0] = function(pc, inst) -- LB
    return ([[
        self.reg[%d] = self.mem[self.reg[%d] + %d]
        if self.reg[%d] >= 0x80 then self.reg[%d] = self.reg[%d] + 0xFFFFFF00 end
    ]]):format(inst.rd, inst.rs1, inst.simm, inst.rd, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][1] = function(pc, inst) -- LH
    return ([[
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256
        else self.reg[%d] = self.mem16[addr / 2] end
        if self.reg[%d] >= 0x8000 then self.reg[%d] = self.reg[%d] + 0xFFFF0000 end
    ]]):format(inst.rs1, inst.simm, inst.rd, inst.rd, inst.rd, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][2] = function(pc, inst) -- LW
    return ([[
        local addr = self.reg[%d] + %d
        if addr %% 4 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256 + self.mem[addr+2] * 65536 + self.mem[addr+3] * 16777216
        else self.reg[%d] = self.mem32[addr / 4] end
    ]]):format(inst.rs1, inst.simm, inst.rd, inst.rd)
end

RISCV.opcodes[0x03][4] = function(pc, inst) -- LBU
    return ([[
        self.reg[%d] = self.mem[self.reg[%d] + %d]
    ]]):format(inst.rd, inst.rs1, inst.simm)
end

RISCV.opcodes[0x03][5] = function(pc, inst) -- LHU
    return ([[
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then self.reg[%d] = self.mem[addr] + self.mem[addr+1] * 256
        else self.reg[%d] = self.mem16[addr / 2] end
    ]]):format(inst.rs1, inst.simm, inst.rd, inst.rd)
end

RISCV.opcodes[0x23][0] = function(pc, inst) -- SB
    return ([[
        self.mem[self.reg[%d] + %d] = self.reg[%d] %% 256
    ]]):format(inst.rs1, inst.simm, inst.rs2)
end

RISCV.opcodes[0x23][1] = function(pc, inst) -- SH
    return ([[
        local addr = self.reg[%d] + %d
        if addr %% 2 ~= 0 then
            self.mem[addr] = bit32.extract(self.reg[%d], 0, 8)
            self.mem[addr+1] = bit32.extract(self.reg[%d], 8, 8)
        else self.mem16[addr / 2] = self.reg[%d] %% 65536 end
    ]]):format(inst.rs1, inst.simm, inst.rs2, inst.rs2, inst.rs2)
end

RISCV.opcodes[0x23][2] = function(pc, inst) -- SW
    return ([[
        local addr = self.reg[%d] + %d
        if addr %% 4 ~= 0 then
            self.mem[addr] = bit32.extract(self.reg[%d], 0, 8)
            self.mem[addr+1] = bit32.extract(self.reg[%d], 8, 8)
            self.mem[addr+2] = bit32.extract(self.reg[%d], 16, 8)
            self.mem[addr+3] = bit32.extract(self.reg[%d], 24, 8)
        else self.mem32[addr / 4] = self.reg[%d] end
    ]]):format(inst.rs1, inst.simm, inst.rs2, inst.rs2, inst.rs2, inst.rs2, inst.rs2)
end

RISCV.opcodes[0x13][0] = function(pc, inst) -- ADDI
    if inst.rs1 == 0 then
        return ([[
        self.reg[%d] = %d
        ]]):format(inst.rd, inst.simm % 0x100000000)
    elseif inst.simm == 0 then
        return ([[
        self.reg[%d] = self.reg[%d]
        ]]):format(inst.rd, inst.rs1)
    else
        return ([[
        self.reg[%d] = (self.reg[%d] + %d) %% 0x100000000
        ]]):format(inst.rd, inst.rs1, inst.simm)
    end
end

RISCV.opcodes[0x13][2] = function(pc, inst) -- SLTI
    return ([[
        local rs = self.reg[%d]
        if rs >= 0x80000000 then rs = rs - 0x100000000 end
        self.reg[%d] = rs < %d and 1 or 0
    ]]):format(inst.rs1, inst.rd, inst.simm)
end

RISCV.opcodes[0x13][3] = function(pc, inst) -- SLTIU
    local imm = inst.simm
    if imm < 0 then imm = imm + 0x100000000 end
    return ([[
        self.reg[%d] = self.reg[%d] < %d and 1 or 0
    ]]):format(inst.rd, inst.rs1, imm)
end

RISCV.opcodes[0x13][4] = function(pc, inst) -- XORI
    return ([[
        self.reg[%d] = bit32.bxor(self.reg[%d], %d)
    ]]):format(inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][6] = function(pc, inst) -- ORI
    return ([[
        self.reg[%d] = bit32.bor(self.reg[%d], %d)
    ]]):format(inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][7] = function(pc, inst) -- ANDI
    return ([[
        self.reg[%d] = bit32.band(self.reg[%d], %d)
    ]]):format(inst.rd, inst.rs1, inst.simm % 0x100000000)
end

RISCV.opcodes[0x13][1] = function(pc, inst) -- SLLI
    return ([[
        self.reg[%d] = bit32.lshift(self.reg[%d], %d)
    ]]):format(inst.rd, inst.rs1, bit32.band(inst.imm, 0x1F))
end

RISCV.opcodes[0x13][5] = function(pc, inst) -- SRLI/SRAI
    return ([[
        self.reg[%d] = bit32.%srshift(self.reg[%d], %d)
    ]]):format(inst.rd, bit32.btest(inst.imm, 0x400) and "a" or "", inst.rs1, bit32.band(inst.imm, 0x1F))
end

RISCV.opcodes[0x33][0] = function(pc, inst) -- ADD/SUB
    return ([[
        self.reg[%d] = (self.reg[%d] %s self.reg[%d]) %% 0x100000000
    ]]):format(inst.rd, inst.rs1, bit32.btest(inst.funct7, 0x20) and "-" or "+", inst.rs2)
end

RISCV.opcodes[0x33][1] = function(pc, inst) -- SLL
    return ([[
        self.reg[%d] = bit32.lshift(self.reg[%d], bit32.band(self.reg[%d], 0x1F))
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][2] = function(pc, inst) -- SLT
    return ([[
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = ra < rb and 1 or 0
    ]]):format(inst.rs1, inst.rs2, inst.rd)
end

RISCV.opcodes[0x33][3] = function(pc, inst) -- SLTU
    return ([[
        self.reg[%d] = self.reg[%d] < self.reg[%d] and 1 or 0
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][4] = function(pc, inst) -- XOR
    return ([[
        self.reg[%d] = bit32.bxor(self.reg[%d], self.reg[%d])
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][5] = function(pc, inst) -- SRL/SRA
    return ([[
        self.reg[%d] = bit32.%srshift(self.reg[%d], bit32.band(self.reg[%d], 0x1F))
    ]]):format(inst.rd, bit32.btest(inst.funct7, 0x20) and "a" or "", inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][6] = function(pc, inst) -- OR
    return ([[
        self.reg[%d] = bit32.bor(self.reg[%d], self.reg[%d])
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x33][7] = function(pc, inst) -- AND
    return ([[
        self.reg[%d] = bit32.band(self.reg[%d], self.reg[%d])
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.opcodes[0x0F] = function(pc, inst) -- FENCE
    -- do nothing
    return ""
end

RISCV.opcodes[0x73] = function(pc, inst) -- ECALL/EBREAK
    if inst.funct3 ~= 0 then return "" end -- Zicsr not implemented
    if inst.imm == 0 then return [=[
        if self.syscalls[self.reg[17]] then self.reg[10] = self.syscalls[self.reg[17]](self, table.unpack(self.reg, 10, 16)) end
        if self.halt then return end
    ]=]
    elseif inst.imm == 0x302 then return [=[
        return self.traces[self.reg[5]](self)
    ]=], true end
end

RISCV.mult_opcodes[0] = function(pc, inst) -- MUL
    return ([[
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        self.reg[%d] = math.abs((ra * rb) %% 0x100000000)
    ]]):format(inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[3] = function(pc, inst) -- MULHU
    return ([[
        self.reg[%d] = math.floor((self.reg[%d] * self.reg[%d]) / 0x100000000)
    ]]):format(inst.rd, inst.rs1, inst.rs2)
end

RISCV.mult_opcodes[2] = function(pc, inst) -- MULHSU
    return ([[
        local ra = self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        local rd = math.floor((ra * self.reg[%d]) / 0x100000000)
        if rd < 0 then rd = rd + 0x100000000 end
        self.reg[%d] = rd
    ]]):format(inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[1] = function(pc, inst) -- MULH
    return ([[
        local ra, rb = self.reg[%d], self.reg[%d]
        if ra >= 0x80000000 then ra = ra - 0x100000000 end
        if rb >= 0x80000000 then rb = rb - 0x100000000 end
        local rd = math.floor((ra * rb) / 0x100000000)
        if rd < 0 then rd = rd + 0x100000000 end
        self.reg[%d] = rd
    ]]):format(inst.rs1, inst.rs2, inst.rd)
end

RISCV.mult_opcodes[4] = function(pc, inst) -- DIV
    return ([[
        if self.reg[%d] == 0 then
            self.reg[%d] = 0xFFFFFFFF
        else
            local ra, rb = self.reg[%d], self.reg[%d]
            if ra >= 0x80000000 then ra = ra - 0x100000000 end
            if rb >= 0x80000000 then rb = rb - 0x100000000 end
            local res = ra / rb
            if res < 0 then self.reg[%d] = math.ceil(res) + 0x100000000
            else self.reg[%d] = math.floor(res) end
        end
    ]]):format(inst.rs2, inst.rd, inst.rs1, inst.rs2, inst.rd, inst.rd)
end

RISCV.mult_opcodes[5] = function(pc, inst) -- DIVU
    return ([[
        if self.reg[%d] == 0 then self.reg[%d] = 0xFFFFFFFF
        else self.reg[%d] = math.floor(self.reg[%d] / self.reg[%d]) end
    ]]):format(inst.rs2, inst.rd, inst.rd, inst.rs1, inst.rs2)
end

RISCV.mult_opcodes[6] = function(pc, inst) -- REM
    return ([[
        if self.reg[%d] == 0 then
            self.reg[%d] = self.reg[%d]
        else
            local ra, rb = self.reg[%d], self.reg[%d]
            if ra >= 0x80000000 then ra = ra - 0x100000000 end
            if rb >= 0x80000000 then rb = rb - 0x100000000 end
            local res = math.fmod(ra, rb)
            if res < 0 then self.reg[%d] = math.ceil(res) + 0x100000000
            else self.reg[%d] = math.floor(res) end
        end
    ]]):format(inst.rs2, inst.rd, inst.rs1, inst.rs1, inst.rs2, inst.rd, inst.rd)
end

RISCV.mult_opcodes[7] = function(pc, inst) -- REMU
    return ([[
        if self.reg[%d] == 0 then self.reg[%d] = self.reg[%d]
        else self.reg[%d] = self.reg[%d] %% self.reg[%d] end
    ]]):format(inst.rs2, inst.rd, inst.rs1, inst.rd, inst.rs1, inst.rs2)
end

RISCV.traces = setmetatable({}, {__index = function(trace, pc)
    local self = RISCV
    local base = pc
    local chunk = ([[
    local math, bit32 = math, bit32
    return function(self)
        if self.halt then return end
        self.branches = self.branches + 1
        if self.branches > self.branchesLimit then coroutine.yield() end
        self.pc = %d
    ]]):format(pc)
    repeat
        if pc >= 33554432 then error("pc out of bounds") end
        if pc % 4 ~= 0 then error(("unaligned jump to %08X"):format(pc)) end
        local inst = self.mem32[pc / 4]
        if inst == 0xc0001073 then chunk = chunk .. "self.halt = true\n" break end
        pc = pc + 4
        local mode = opcode_modes[bit32.band(inst, 0x7F)]
        if not mode then
            error(("Unknown opcode %02X at %08X"):format(bit32.band(inst, 0x7F), pc - 4))
            return
        end
        inst = mode(inst)
        --print(textutils.serialize(inst))
        local f = self.opcodes[inst.opcode]
        local op, isBranch
        if type(f) == "function" then op, isBranch = f(pc, inst)
        elseif type(f) == "table" then
            if not f[inst.funct3] then print("Unknown function " .. inst.funct3)
            elseif inst.opcode == 0x33 and bit32.btest(inst.funct7, 1) then op, isBranch = self.mult_opcodes[inst.funct3](pc, inst)
            else op, isBranch = f[inst.funct3](pc, inst) end
        else error("Unknown opcode " .. inst.opcode .. " at " .. (pc - 4)) end
        chunk = chunk .. op
    until isBranch
    chunk = chunk .. "end"
    --print(chunk)
    local fn = assert(load(chunk, ("@%08X"):format(base)))()
    trace[base] = fn
    return fn
end})

function RISCV:run(cycles)
    self.branches = 0
    self.branchesLimit = cycles
    if self.coro then
        assert(coroutine.resume(self.coro))
    else
        self.coro = coroutine.create(self.traces[self.pc])
        assert(coroutine.resume(self.coro, self))
    end
    if coroutine.status(self.coro) == "dead" then self.coro = nil end
end

local function loadELF(data, mem)
    RISCV.traces = setmetatable({}, getmetatable(RISCV.traces))
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
local test = ...
for _, name in ipairs(fs.list("doom/tests")) do
    if not test or name == test then
    print(name)
    local file = fs.open("doom/tests/" .. name, "rb")
    local elf = file.readAll()
    file.close()
    RISCV.halt = false
    RISCV.pc = loadELF(elf, RISCV.mem)
    RISCV.reg[2] = 0x1FF0000
    assert(xpcall(function()
        while not RISCV.halt do RISCV:run(10000) end
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
end
--]=]