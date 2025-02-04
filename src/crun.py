cruntime = """
--// cruntime.lua \\\\--
-- This file is apart of the RCC project.
-- specifically the roblox-c compiler.

-- CASTING
local function cast(type, value)
    type = type:gsub(" long", ""):gsub(" short", "") -- Fix: Only remove " long" and " short" with space
    if type == "int" then
        return tonumber(value)
    elseif type == "string" then
        return tostring(value)
    elseif type == "char" then
        return string.sub(tostring(value), 1, 1) -- Correct char casting to single char
    elseif type == "float" then
        return tonumber(value)
    elseif type == "double" then
        return tonumber(value)
    elseif type == "bool" then
        return not not value -- More concise boolean conversion
    else
        error("[roblox-c] c-like-casting error: type '" .. type .. "' is not a supported type.")
    end
end

-- BIT
local bitlib = require("bit") -- Assume 'bit' library is available, make it explicit

local function create_bit_op(bit_op)
    return function()
        local n1 = nil
        local n2 = nil
        local ins = {}
        local meta = {
            __div = function(_, other) -- Fix: Remove self, not used
                n1 = other
                return ins
            end,
            __mul = function(_, other) -- Fix: Remove self, not used
                n2 = other
                if n1 == nil or n2 == nil then
                    error("[roblox-c] bit operation error: operands not set correctly (N1/bit.op*N2)")
                end
                return bit_op(n1, n2)
            end,
        }
        setmetatable(ins, meta)
        return ins
    end
end

local band = create_bit_op(bitlib.band)
local bxor = create_bit_op(bitlib.bxor)
local bor = create_bit_op(bitlib.bor)
local bitlshift = create_bit_op(bitlib.lshift)
local bitrshift = create_bit_op(bitlib.rshift)

-- SWITCH
local def = newproxy()
local brk = newproxy()

local function switch(value, cases)
    if cases[value] then
        local case_result = cases[value]()
        if case_result == brk then -- Fix: Only break if explicitly returns brk
            return nil -- Indicate break
        elseif case_result ~= nil then -- Fix: If case returns a value, return it and stop. No fallthrough in this case.
            return case_result
        else
            -- Fallthrough logic: execute subsequent cases until brk or end of cases.
            local do_fallthrough = true
            local case_index = nil
            local case_keys = {}
            for k in pairs(cases) do table.insert(case_keys, k) end
            table.sort(case_keys, function(a, b) return tostring(a) < tostring(b) end) -- Ensure consistent order

            for i, k in ipairs(case_keys) do
                if k == value then
                    case_index = i
                    break
                end
            end

            if case_index then
                for i = case_index + 1, #case_keys do
                    local next_case_result = cases[case_keys[i]]()
                    if next_case_result == brk then
                        do_fallthrough = false
                        break
                    elseif next_case_result ~= nil then
                        -- if next case returns value, stop fallthrough and return the value.
                        return next_case_result -- early return, no further fallthrough
                    end
                end
            end
            if not do_fallthrough and case_result == brk then
                return nil -- Explicitly return nil for break;
            end
        end
    elseif cases[def] then
        return cases[def]() -- Fix: Return the result of default case
    end
    return nil -- Default return if no case matched and no default case.
end


-- C++
local construct = newproxy()
local destruct = newproxy()

local function new(methods, ...)
    local obj = setmetatable({}, methods) -- Create instance and set metatable
    if methods[construct] then
        methods[construct](obj, ...) -- Pass obj to constructor
    end
    return obj
end

local function delete(obj)
    if obj and obj[destruct] then -- Check if obj is not nil before accessing destruct
        obj[destruct](obj) -- Pass obj to destructor
    end
end


-- MEMORY
-- todo: support directly modifying memory
_G.CMemory = _G.CMemory or {}
local CMemory = _G.CMemory -- Localize access to global

local function malloc(size)
    local ptr_index = #CMemory + 1
    CMemory[ptr_index] = table.create(size, nil) -- Initialize with nil, like uninitialized memory
    return ptr_index
end

local function free(ptr)
    CMemory[ptr] = nil
end

local function realloc(ptr, size)
    if not CMemory[ptr] then -- Handle invalid pointer
        return malloc(size) -- If ptr is invalid, just allocate new memory
    end
    local old_table = CMemory[ptr]
    local new_table = table.create(size, nil) -- Create new table
    local copy_size = math.min(size, #old_table) -- Determine copy size
    for i = 1, copy_size do -- Copy existing content
        new_table[i] = old_table[i]
    end
    CMemory[ptr] = new_table -- Replace with new table at the same ptr
    return ptr -- realloc should return the same pointer if possible
end


local function calloc(size)
    local ptr_index = #CMemory + 1
    CMemory[ptr_index] = table.create(size, 0) -- Initialize with 0, like calloc
    return ptr_index
end

local function memset(ptr, value, size)
    if not CMemory[ptr] then return end -- Handle invalid pointer
    local mem_block = CMemory[ptr]
    for i = 1, size do
        mem_block[i] = value
    end
end

local function memcpy(dest, src, size)
    if not CMemory[dest] or not CMemory[src] then return end -- Handle invalid pointers
    local dest_block = CMemory[dest]
    local src_block = CMemory[src]
    for i = 1, size do
        dest_block[i] = src_block[i]
    end
end

local function memmove(dest, src, size)
    if not CMemory[dest] or not CMemory[src] then return end -- Handle invalid pointers
    local dest_block = CMemory[dest]
    local src_block = CMemory[src]
    if dest == src then return end -- No move needed
    if dest > src and dest < src + size then -- Overlapping regions, move from end
        for i = size, 1, -1 do
            dest_block[i] = src_block[i]
        end
    else -- Non-overlapping or src before dest, normal copy
        for i = 1, size do
            dest_block[i] = src_block[i]
        end
    end
end


local function memcmp(ptr1, ptr2, size)
    if not CMemory[ptr1] or not CMemory[ptr2] then return false end -- Handle invalid pointers
    local block1 = CMemory[ptr1]
    local block2 = CMemory[ptr2]
    for i = 1, size do
        if block1[i] ~= block2[i] then
            return false
        end
    end
    return true
end

local function memchr(ptr, value, size)
    if not CMemory[ptr] then return false end -- Handle invalid pointer
    local mem_block = CMemory[ptr]
    for i = 1, size do
        if mem_block[i] == value then
            return true
        end
    end
    return false
end

local function ptr(value)
    local ptr_index = #CMemory + 1
    CMemory[ptr_index] = {value} -- Store value in a table
    return ptr_index
end

local function deref(ptr)
    if not CMemory[ptr] then return nil end -- Handle invalid pointer
    local mem_block = CMemory[ptr]
    return mem_block[1] -- Return the value, not the table
end

local function concat(str, str2)
    return str .. str2
end


return {
    -- INTERNAL
    cast = cast,
    switch = switch,
    new = new,
    delete = delete,
    ptr = ptr,
    deref = deref,
    band = band,
    bxor = bxor,
    bor = bor,
    bitlshift = bitlshift,
    bitrshift = bitrshift,

    -- STD
    malloc = malloc,
    free = free,
    realloc = realloc,
    calloc = calloc,
    memset = memset,
    memcpy = memcpy,
    memmove = memmove,
    memcmp = memcmp,
    memchr = memchr,
    printf = function(str, ...) print(str:format(...)) end,
    concat = concat,

    -- CONSTANTS
    construct = construct,
    destruct = destruct,
    def = def,
    brk = brk
}
"""
