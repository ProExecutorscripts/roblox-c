# crun.py
cruntime = """
--// cruntime.luau \\\\-- -- Changed extension to .luau and header comment to reflect Luau
-- This file is apart of the RCC project.
-- specifically the roblox-c compiler.

--!strict -- Added Luau strict mode

-- CASTING
function cast(type: string, value: any): any -- Added type annotations for Luau
    type = type:gsub("long", ""):gsub("short", "")
    if type == "int" then
        return tonumber(value)
    elseif type == "string" then
        return tostring(value)
    elseif type == "char" then
        return tostring(value)
    elseif type == "float" then
        return tonumber(value)
    elseif type == "double" then
        return tonumber(value)
    elseif type == "bool" then
        if value then
            return true
        else
            return false
        end
    else
        error(("[roblox-c] c-like-casting error: type '" .. type .. "' is not a supported type."))
    end
end

-- BIT
-- bit functions are like N1/C.bit*N2
function band(): any -- Added type annotations for Luau
    local n1: number? = nil -- Added type annotations for Luau
    local n2: number? = nil -- Added type annotations for Luau
    local ins: {} = {} -- Added type annotations for Luau
    local meta = {
        __div = function(self: {}, other: number): {} -- Added type annotations for Luau
            n1 = other
            return self
        end,
        __mul = function(self: {}, other: number): number -- Added type annotations for Luau
            n2 = other
            if bit then -- Check if 'bit' library exists (may need to polyfill if not in Roblox Luau)
                return bit.band(n1, n2)
            else
                error("[roblox-c] bit library is required for bitwise operations in cruntime.luau")
                return 0 -- Or handle appropriately if 'bit' is not available
            end
        end,
    }
    setmetatable(ins, meta)
    return ins
end
function bxor(): any -- Added type annotations for Luau
    local n1: number? = nil -- Added type annotations for Luau
    local n2: number? = nil -- Added type annotations for Luau
    local ins: {} = {} -- Added type annotations for Luau
    local meta = {
        __div = function(self: {}, other: number): {} -- Added type annotations for Luau
            n1 = other
            return self
        end,
        __mul = function(self: {}, other: number): number -- Added type annotations for Luau
            n2 = other
            if bit then -- Check if 'bit' library exists
                return bit.bxor(n1, n2)
            else
                error("[roblox-c] bit library is required for bitwise operations in cruntime.luau")
                return 0 -- Or handle appropriately
            end
        end,
    }
    setmetatable(ins, meta)
    return ins
end
function bor(): any -- Added type annotations for Luau
    local n1: number? = nil -- Added type annotations for Luau
    local n2: number? = nil -- Added type annotations for Luau
    local ins: {} = {} -- Added type annotations for Luau
    local meta = {
        __div = function(self: {}, other: number): {} -- Added type annotations for Luau
            n1 = other
            return self
        end,
        __mul = function(self: {}, other: number): number -- Added type annotations for Luau
            n2 = other
            if bit then -- Check if 'bit' library exists
                return bit.bor(n1, n2)
            else
                error("[roblox-c] bit library is required for bitwise operations in cruntime.luau")
                return 0 -- Or handle appropriately
            end
        end,
    }
    setmetatable(ins, meta)
    return ins
end
function bitlshift(): any -- Added type annotations for Luau
    local n1: number? = nil -- Added type annotations for Luau
    local n2: number? = nil -- Added type annotations for Luau
    local ins: {} = {} -- Added type annotations for Luau
    local meta = {
        __div = function(self: {}, other: number): {} -- Added type annotations for Luau
            n1 = other
            return self
        end,
        __mul = function(self: {}, other: number): number -- Added type annotations for Luau
            n2 = other
            if bit then -- Check if 'bit' library exists
                return bit.lshift(n1, n2)
            else
                error("[roblox-c] bit library is required for bitwise operations in cruntime.luau")
                return 0 -- Or handle appropriately
            end
        end,
    }
    setmetatable(ins, meta)
    return ins
end
function bitrshift(): any -- Added type annotations for Luau
    local n1: number? = nil -- Added type annotations for Luau
    local n2: number? = nil -- Added type annotations for Luau
    local ins: {} = {} -- Added type annotations for Luau
    local meta = {
        __div = function(self: {}, other: number): {} -- Added type annotations for Luau
            n1 = other
            return self
        end,
        __mul = function(self: {}, other: number): number -- Added type annotations for Luau
            n2 = other
            if bit then -- Check if 'bit' library exists
                return bit.rshift(n1, n2)
            else
                error("[roblox-c] bit library is required for bitwise operations in cruntime.luau")
                return 0 -- Or handle appropriately
            end
        end,
    }
    setmetatable(ins, meta)
    return ins
end
-- SWITCH
local def: any = newproxy() -- Added type annotations for Luau
local brk: any = newproxy() -- Added type annotations for Luau

function switch(value: any, cases: { [any]: () -> any? }): any -- Added type annotations for Luau
    if cases[value] then
        local returnv = cases[value]()
        if returnv ~= nil then
            -- fallthrough
            local start = false
            for i, v in cases do
                if i == value then
                    start = true
                end
                if start then
                    v()
                end
            end
        elseif returnv == brk then
            return nil
        else
            return returnv
        end
    elseif cases[def] then
        cases[def]()
    end
end

-- C++
local construct: any = newproxy() -- Added type annotations for Luau
local destruct: any = newproxy() -- Added type annotations for Luau

function new(methods: { [any]: (...any) -> any }, ...: any): {} -- Added type annotations for Luau
    if methods[construct] then
        methods[construct](...)
    end
    return methods
end
function delete(obj: { [any]: (...any) -> any }): nil -- Added type annotations for Luau
    if obj[destruct] then
        obj[destruct]()
    end
    return nil -- Explicitly return nil for void function in Luau
end

-- MEMORY
-- todo: support directly modifying memory
_G.CMemory = _G.CMemory or {}

function malloc(size: number): number -- Added type annotations for Luau
    _G.CMemory[#_G.CMemory+1] = table.create(size)
    return #_G.CMemory
end
function free(ptr: number): nil -- Added type annotations for Luau
    _G.CMemory[ptr] = nil
    return nil -- Explicitly return nil for void function in Luau
end
function realloc(ptr: number, size: number): number -- Added type annotations for Luau
    _G.CMemory[ptr] = table.create(size)
    return #_G.CMemory
end
function calloc(size: number): number -- Added type annotations for Luau
    _G.CMemory[#_G.CMemory+1] = table.create(size)
    return #_G.CMemory
end
function memset(ptr: number, value: any, size: number): nil -- Added type annotations for Luau
    for i = 1, size do
        _G.CMemory[ptr][i] = value
    end
    return nil -- Explicitly return nil for void function in Luau
end
function memcpy(dest: number, src: number, size: number): nil -- Added type annotations for Luau
    for i = 1, size do
        _G.CMemory[dest][i] = _G.CMemory[src][i]
    end
    return nil -- Explicitly return nil for void function in Luau
end
function memmove(dest: number, src: number, size: number): nil -- Added type annotations for Luau
    for i = 1, size do
        _G.CMemory[dest][i] = _G.CMemory[src][i]
    end
    return nil -- Explicitly return nil for void function in Luau
end
function memcmp(ptr1: number, ptr2: number, size: number): boolean -- Added type annotations for Luau
    for i = 1, size do
        if _G.CMemory[ptr1][i] ~= _G.CMemory[ptr2][i] then
            return false
        end
    end
    return true
end
function memchr(ptr: number, value: any, size: number): boolean -- Added type annotations for Luau
    for i = 1, size do
        if _G.CMemory[ptr][i] == value then
            return true
        end
    end
    return false
end

function ptr(value: any): number -- Added type annotations for Luau
    local index = #_G.CMemory + 1
    _G.CMemory[index] = {value}
    return index
end
function deref(ptr: number): any -- Added type annotations for Luau
    if _G.CMemory[ptr] then
        return _G.CMemory[ptr][1]
    else
        return nil -- Or handle invalid pointer access as needed
    end
end
function concat(str: string, str2: string): string -- Added type annotations for Luau
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
    printf = function(str: string, ...: any) print(str:format(...)) end, -- Added type annotations for Luau
    concat = concat,

    -- CONSTANTS
    construct = construct,
    destruct = destruct,
    def = def,
    brk = brk
}
"""

# rbx.h - No changes needed for rbx.h as it's C/C++ header

# rbxc.py
rbxc_py = """
import sys, os, json
import clang.cindex as clang
from clang.cindex import Config
import luaparser
from luaparser.ast import *
from luaparser.astnodes import *

#### LOG #####
def error(msg):
    sys.stderr.write("\\033[91;1merror\\033[0m \\033[90mLuau roblox-c:\\033[0m " + msg + "\\n") # Changed log prefix to Luau
    sys.exit(1)
def warn(msg):
    sys.stderr.write("\\033[1;33m" + "warning: " + "\\033[0m" + "\\033[90mLuau roblox-c:\\033[0m " + msg + "\\n") # Changed log prefix to Luau
def info(msg):
    sys.stderr.write("\\033[1;32m" + "info: " + "\\033[0m" + "\\033[90mLuau roblox-c:\\033[0m " + msg  + "\\n") # Changed log prefix to Luau


#### CONSTANTS ####
CONFIG_FILE = os.path.expanduser('~/.config/rbxc/config.json')
VERSION = "1.2.0-luau" # Updated version to indicate Luau support
TAB = "\\t\\b\\b\\b\\b"

#### DEPENDENCIES ####
try:
    import clang.cindex as clang
    from clang.cindex import Config
except ImportError:
    error("libclang could not be resolved. Please install it or configure libclang path using '-p'")

try:
    import luaparser
    from luaparser.ast import *
    from luaparser.astnodes import *
except ImportError:
    error("luaparser could not be resolved. Please install it using 'pip install luaparser'")

#### COMPILER PARSER ####
bins = {
    "!": "not ",
    "||": " or ",
    "&&": " and ",
    "==": "==",
    "!=": "~=",
    "<": "<",
    ">": ">",
    "<=": "<=",
    ">=": ">=",
    "**":"^",
    "]": " + 1] ",
    "<<": "/C.bitlshift()*",
    ">>": "/C.bitrshift()*",
    "|": "/C.bor()*",
    "&": "/C.band()*",
    "^": "/C.bxor()*",
    "+=": "+=",
    "-=": "-=",
    "*=": "*=",
    "/=": "/=",
    "%=": "%=",
    "&=": "/C.band() *",
    "|=": "/C.bor() *",
    "^=": "/C.bxor() *",
    "<<=": "/C.bitlshift() *",
    ">>=": "/C.bitrshift() *",
    "=": "=",
    "+": "+",
    "-": "-",
    "*": "*",
    "/": "/",
    "%": "%",
    ".": ".",
    "->": ".",
}
uns = {
    "++": {
        "v":" += 1",
        "wrap": False,
    },
    "**": {
        "v":"^",
        "wrap": False,
    },
    "--": {
        "v":" -= 1",
        "wrap": False,
    },
    "&": {
        "v":"C.ptr(",
        "wrap": True,
    },
    "*": {
        "v":"C.deref(",
        "wrap": True,
    },
    "!": {
        "v":"not ",
        "wrap": False,
    },
    "sizeof": {
        "v":"C.sizeof(", # not implemented in cruntime yet
        "wrap": True,
    },
    "sizeof...": {
        "v":"C.sizeof(", # not implemented in cruntime yet
        "wrap": True,
    },
    "alignof": {
        "v":"C.alignof(", # not implemented in cruntime yet
        "wrap": True,
    },
    "__real": {
        "v":"C.__real(", # not implemented in cruntime yet
        "wrap": True,
    },
    "__imag": {
        "v":"C.__imag(", # not implemented in cruntime yet
        "wrap": True,
    },
    "-": {
        "v": "-",
        "wrap": False,
    },
    "+": {
        "v": "+",
        "wrap": False,
    },
    "~": {
        "v": "~", # not implemented in cruntime yet
        "wrap": False,
    }
}
types = {
    "int": "number", # Mapped types to Luau types
    "char": "string", # Luau strings are UTF-8, can represent chars
    "float": "number",
    "double": "number",
    "bool": "boolean", # Luau boolean type
    "void": "nil", # Luau nil for void
    "long": "number",
    "short": "number",
    "unsigned int": "number",
    "unsigned char": "string",
    "unsigned long": "number",
    "unsigned short": "number",
    "signed int": "number",
    "signed char": "string",
    "signed long": "number",
    "signed short": "number",
    "const char *": "string",
    "const char*": "string",
    "char *": "string",
    "char*": "string",
    "string": "string",
    "size_t": "number", # Assuming size_t is an unsigned integer type
}


def get_ast(file_path, c, check=True, flags=[]):
    if check:
        test(file_path, c, flags)
    try:
        index = clang.Index.create()
        if c:
            std = "c23" # Updated C standard
        else:
            std = "c++26" # Updated C++ standard
        tu_flags = ['-std='+std, '-D__RCC__=1'] + flags
        translation_unit = index.parse(file_path, args=tu_flags)
        return translation_unit.cursor
    except clang.LibclangError as e:
        error("libclang error: " + str(e) + ". Is libclang installed and configured correctly? Use '-p [path to libclang.dylib or libclang.so]' to set the path.")
        sys.exit(1)
    except Exception as e:
        error(str(e))
        sys.exit(1)
def print_ast(node, depth=0):
    print('  ' * depth + str(node.kind) + ' : ' + node.spelling + " -- " + str(node.type.spelling))
    for child in node.get_children():
        print_ast(child, depth + 1)
def test(file_path, c, flags):
    # Runs gcc on the file to check for errors, if there is an error sys.exit(1)
    if c:
        iserrors = os.system("gcc -fsyntax-only -std=c23 -D__RCC__=1 " + " ".join(flags) + " " + file_path + " 2> error.log") # Updated C standard for test
    else:
        iserrors = os.system("g++ -fsyntax-only -std=c++26 -D__RCC__=1 " + " ".join(flags) + " " + file_path + " 2> error.log") # Updated C++ standard for test
    if iserrors != 0:
        with open("error.log", "r") as f:
            error_log = f.read()
        os.remove("error.log")
        error("Compilation failed with errors:\\n" + error_log)


#### GENERATOR ####
class NodeVisitor(object):
    def __init__(self, isC):
        self.indent = 0
        self.ast = None
        if isC:
            self.lang = "C"
        else:
            self.lang = "C++"
            self.namespaces = {}
        self.current_function_name = None
        self.local_vars = {} # Track local variables in functions

    ### VISITORS ###
    # CORE
    def visit_translation_unit(self, node):
        body = []
        for child in node.get_children():
            res = self.visit(child)
            if res: # Ignore None results (like comments)
                if isinstance(res, list):
                    body.extend(res) # Extend if it's a list of nodes
                else:
                    body.append(res)
        self.ast = Chunk(Block(body))

    # CONTROL FLOW
    def visit_function_decl(self, node):
        params = []
        body_nodes = []
        self.current_function_name = node.spelling
        self.local_vars[self.current_function_name] = set() # Initialize local vars for this function
        arg_types = {} # To store argument types for Luau type annotations (if needed later)

        for child in node.get_children():
            if child.kind == clang.CursorKind.PARM_DECL:
                param_type_c = child.type.spelling
                param_type_luau = types.get(param_type_c, 'any') # Get Luau type, default to 'any'
                params.append(child.spelling) # Only parameter name needed in Luau
                # arg_types[child.spelling] = param_type_luau -- Store type if we want to add annotations later
                self.local_vars[self.current_function_name].add(child.spelling) # Add param to local vars
            elif child.kind == clang.CursorKind.COMPOUND_STMT:
                body_nodes = [self.visit(c) for c in child.get_children() if self.visit(c) is not None] # Filter out None returns

        func_name = node.spelling
        function_node = LocalFunction(
            name=func_name,
            args=params,
            body=Block(body_nodes)
        )
        self.current_function_name = None # Reset current function name
        return function_node


    def visit_compound_stmt(self, node):
        block = []
        for child in node.get_children():
            res = self.visit(child)
            if res:
                if isinstance(res, list):
                    block.extend(res)
                else:
                    block.append(res)
        return Block(block)

    def visit_return_stmt(self, node):
        return Return(values=[self.visit(child) for child in node.get_children() if self.visit(child) is not None])

    def visit_break_stmt(self, node):
        return Break()

    def visit_if_stmt(self, node):
        parts = []
        children = list(node.get_children())
        clause = {}

        if_condition = self.visit(children[0])
        if_body = self.visit(children[1])
        clause['if'] = [if_condition, if_body]

        if len(children) > 2:
            else_node = children[2]
            if else_node.kind == clang.CursorKind.IF_STMT: # else if
                clause['elseif'] = self.visit(else_node).body.body # Unwrap from IfThenElse
            else: # else
                clause['else'] = self.visit(else_node)

        return IfThenElse(body=[clause])


    def visit_for_stmt(self, node):
        parts = list(node.get_children())
        init = self.visit(parts[0]) if parts[0].kind != clang.CursorKind.NULL_STMT else None # Init statement
        condition = self.visit(parts[1]) if parts[1].kind != clang.CursorKind.NULL_STMT else None # Condition expression
        increment = self.visit(parts[2]) if parts[2].kind != clang.CursorKind.NULL_STMT else None # Increment expression
        body = self.visit(parts[3]) # Loop body

        init_block = Block([init]) if init else Block([])
        increment_block = Block([increment]) if increment else Block([])

        return While(
            condition=condition,
            body=Block([
                init_block,
                body,
                increment_block
            ])
        )


    def visit_while_stmt(self, node):
        children = list(node.get_children())
        condition = self.visit(children[0])
        body = self.visit(children[1])
        return While(condition=condition, body=body)

    def visit_do_stmt(self, node):
        children = list(node.get_children())
        body = self.visit(children[0])
        condition = self.visit(children[1])
        return Repeat(body=body, condition=condition)


    def visit_switch_stmt(self, node):
        cases = {}
        default_case = None
        for child in node.get_children():
            if child.kind == clang.CursorKind.CASE_STMT:
                case_value_node = next(child.get_children()) # Get the case value
                case_value = self.visit(case_value_node)
                case_body_nodes = [self.visit(c) for c in child.get_children() if c != case_value_node] # Get case body, excluding value
                cases[case_value] = Block(case_body_nodes)
            elif child.kind == clang.CursorKind.DEFAULT_STMT:
                default_case_body_nodes = [self.visit(c) for c in child.get_children()]
                default_case = Block(default_case_body_nodes)

        switch_value_node = next(node.get_children()) # First child is the switch value
        switch_value = self.visit(switch_value_node)

        case_table = Table()
        case_table.fields = []
        for case_val, case_block in cases.items():
            case_table.fields.append(TableKey(key=case_val, value=Function(args=[], body=case_block)))
        if default_case:
             case_table.fields.append(TableKey(key=Name(id='C.def'), value=Function(args=[], body=default_case)))


        return Call(func=Name(id='C.switch'), args=[switch_value, case_table])


    # EXPRESSIONS and STATEMENTS
    def visit_decl_stmt(self, node):
        declarations = []
        for child in node.get_children():
            if child.kind == clang.CursorKind.VAR_DECL:
                declarations.append(self.visit_var_decl(child))
            elif child.kind == clang.CursorKind.FIELD_DECL: # For struct/class members (if needed)
                declarations.append(self.visit_field_decl(child)) # Implement visit_field_decl if needed
        return declarations

    def visit_var_decl(self, node):
        var_name = node.spelling
        var_type = node.type.spelling
        lua_type = types.get(var_type, 'any') # Get Luau equivalent type, default to 'any'
        init_value_node = next(node.get_children(), None) # Get initializer if any
        init_value = self.visit(init_value_node) if init_value_node else None

        self.local_vars.setdefault(self.current_function_name, set()).add(var_name) # Track local variable

        if init_value:
            return Assign([Name(id=var_name)], ['='], [init_value])
        else:
            return Local(names=[var_name]) # Local declaration without assignment



    def visit_binary_operator(self, node):
        lhs_node, rhs_node = node.get_children()
        lhs = self.visit(lhs_node)
        rhs = self.visit(rhs_node)
        opcode = node.spelling.replace(lhs_node.spelling, '', 1).replace(rhs_node.spelling, '', 1).strip() # Extract opcode
        lua_op = bins.get(opcode, opcode) # Map C opcode to Luau opcode, default to C opcode if no mapping

        if lua_op.startswith("/C."): # Special case for cruntime functions (bit operations, etc.)
            parts = lua_op.split("*")
            c_func = parts[0][1:] # Extract C function name (e.g., C.bitlshift())
            return Call(func=Index(value=Name(id='C'), key=Name(id=c_func)), args=[lhs, rhs])
        elif lua_op in ["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "="]: # Assignment operators
             return Assign([lhs], [lua_op], [rhs]) # Handle assignment as Assign node
        else:
            return BinOp(op=lua_op, left=lhs, right=rhs)


    def visit_unary_operator(self, node):
        operand_node = next(node.get_children())
        operand = self.visit(operand_node)
        opcode = node.spelling.replace(operand_node.spelling, '', 1).strip() # Extract opcode
        op_data = uns.get(opcode)

        if op_data:
            lua_op = op_data["v"]
            if op_data["wrap"]:
                return Call(func=Name(id=lua_op[:-1]), args=[operand]) # Call cruntime function (e.g., C.ptr())
            else:
                if opcode in ["++", "--"]: # Increment/Decrement
                    return Assign([operand], [lua_op[:3]], [Number(n='1')]) # e.g., x += 1
                else:
                    return UnOp(op=lua_op, exp=operand) # e.g., not x

        return UnOp(op=opcode, exp=operand) # Default if no mapping


    def visit_call_expr(self, node):
        callee_node = next(node.get_children())
        args_nodes = [arg for arg in node.get_children()][1:]
        callee = self.visit(callee_node)
        args = [self.visit(arg) for arg in args_nodes]

        if callee_node.kind == clang.CursorKind.DECL_REF_EXPR and callee_node.spelling == 'printf':
            # Special case for printf to handle varargs
            if args:
                format_str = args[0]
                printf_args = args[1:]
                return Call(func=Index(value=Name(id='C'), key=Name(id='printf')), args=[format_str] + printf_args)
            else:
                return Call(func=Index(value=Name(id='C'), key=Name(id='printf')), args=[String(s='')]) # printf with no args

        elif callee_node.kind == clang.CursorKind.DECL_REF_EXPR and callee_node.spelling in ['malloc', 'free', 'realloc', 'calloc', 'memset', 'memcpy', 'memmove', 'memcmp', 'memchr', 'concat']:
            return Call(func=Index(value=Name(id='C'), key=Name(id=callee_node.spelling)), args=args) # Call cruntime stdlib functions

        elif callee_node.kind == clang.CursorKind.TYPE_REF and callee_node.type.kind == clang.TypeKind.TYPEDEF: # Casting (e.g., (int)x)
            cast_type = callee_node.type.spelling # Get type name from typeref
            if args:
                value_to_cast = args[0] # Assume first arg is value to cast
                return Call(func=Index(value=Name(id='C'), key=Name(id='cast')), args=[String(s=cast_type), value_to_cast])


        return Call(func=callee, args=args) # Regular function call


    def visit_decl_ref_expr(self, node):
        var_name = node.spelling
        if self.current_function_name and var_name in self.local_vars[self.current_function_name]:
            return Name(id=var_name) # Local variable
        else:
            return Name(id=var_name) # Assume global or external


    def visit_member_ref_expr(self, node):
        base_node = next(node.get_children())
        base = self.visit(base_node)
        field_name = node.spelling.replace(base_node.spelling + ".", "") if "." in node.spelling else node.spelling.replace(base_node.spelling + "->", "") # Handle both . and ->

        return Index(value=base, key=String(s=field_name)) # Assuming struct/class member access


    def visit_array_subscript_expr(self, node):
        base_node, index_node = node.get_children()
        base = self.visit(base_node)
        index = self.visit(index_node)
        return Index(value=base, key=index) # Array access

    # LITERALS
    def visit_integer_literal(self, node):
        return Number(n=node.spelling)
    def visit_floating_literal(self, node):
        return Number(n=node.spelling)
    def visit_character_literal(self, node):
        return String(s=node.spelling)
    def visit_string_literal(self, node):
        return String(s=node.spelling)

    def visit_type_ref(self, node):
        return Name(id=types.get(node.type.spelling, node.type.spelling)) # For type names (e.g., in casts)

    def visit_paren_expr(self, node):
        return Group(exp=self.visit(list(node.get_children())[0])) # Handle parentheses

    def visit_null_stmt(self, node):
        return None # Ignore null statements (empty ;)

    def visit_typedef_decl(self, node):
        # For now, just ignore typedefs, types are handled in 'types' dict
        return None

    def visit_enum_decl(self, node):
        # For now, ignore enums, could be implemented as Luau tables if needed
        return None

    def visit_enum_constant_decl(self, node):
        # For now, ignore enum constants
        return None

    def visit_struct_decl(self, node):
        # For now, ignore struct declarations
        return None

    def visit_union_decl(self, node):
        # For now, ignore union declarations
        return None

    def visit_field_decl(self, node):
        # For now, ignore field declarations in structs/classes
        return None

    def visit_namespace(self, node):
        # For C++, handle namespaces if needed
        namespace_name = node.spelling
        self.namespaces[namespace_name] = {} # Store namespace content if needed
        namespace_body = Block([self.visit(child) for child in node.get_children()]) # Process namespace content
        return namespace_body # Or handle namespace as needed

    def visit_namespace_alias(self, node):
        # Handle namespace aliases if relevant
        return None

    def visit_using_directive(self, node):
        # Handle using namespace directives if relevant
        return None

    def visit_static_assert_decl(self, node):
        # Ignore static asserts for now
        return None

    def visit_unexposed_stmt(self, node):
        # Catch-all for unexposed statements, try to visit children
        body = []
        for child in node.get_children():
            res = self.visit(child)
            if res:
                if isinstance(res, list):
                    body.extend(res)
                else:
                    body.append(res)
        return body if body else None

    def visit_unexposed_expr(self, node):
        # Catch-all for unexposed expressions, try to visit children
        children_results = [self.visit(child) for child in node.get_children()]
        return children_results[0] if children_results else None # Return the first valid result or None


    ### NODESYSTEM ###
    def visit(self, node):
        method = 'visit_' + node.kind.name.lower()
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)
    def generic_visit(self, node):
        warn(f"{node.kind.name.lower()} not fully implemented, or ignored for now at {node.location}. Output might be incorrect.")
        return None # Or generate a comment node: Comment(node.kind.name.lower() + " not implemented")
    def gen(self):
        return to_lua_source(self.ast)

#### HEADER ####
REQUIRE = "local C = require(game.ReplicatedStorage:WaitForChild(\"Packages\").cruntime)\\n\\n" # No change needed for require path
HEADER = "--// Generated by roblox-c v" + VERSION + " for Luau \\\\--\\n--!strict\\n"+REQUIRE # Updated header comment to mention Luau and added !strict
LIBS = ["malloc", "free", "realloc", "calloc", "memset", "memcpy", "memmove", "memcmp", "memchr", "printf", "concat", "ptr", "deref", "cast", "band", "bxor", "bor", "bitlshift", "bitrshift", "switch", "new", "delete"] # Include all cruntime functions used in generated code

def gen_header_code(code):
    header_code = ""
    used_libs = set()
    for lib in LIBS:
        if lib in code:
            used_libs.add(lib)
    for lib in sorted(list(used_libs)): # Sort for consistent output
        header_code += lib+" = C."+lib+"\\n"
    return header_code

#### INTERFACE ####
def check():
    if not os.path.exists(os.path.expanduser('~/.config/rbxc')):
        os.makedirs(os.path.expanduser('~/.config/rbxc'))
    if not os.path.exists(CONFIG_FILE):
        with open(CONFIG_FILE, 'w') as f:
            json.dump({}, f)
def config(key, value):
    check()
    # Load existing config file or create a new one if it doesn't exist
    if os.path.exists(CONFIG_FILE):
        with open(CONFIG_FILE, 'r') as f:
            config_data = json.load(f)
    else:
        config_data = {}

    # Update the config with the new key-value pair
    config_data[key] = value

    # Save the updated config to disk
    with open(CONFIG_FILE, 'w') as f:
        json.dump(config_data, f)
def isconfig(key):
    check()
    # Load the config file
    if os.path.exists(CONFIG_FILE):
        with open(CONFIG_FILE, 'r') as f:
            config_data = json.load(f)
    else:
        config_data = {}

    # Return the value of the specified key, or None if it doesn't exist
    return config_data.get(key)

def usage():
    print("\\n"+f\"\"\"usage: \\033[1;94mrbxc\\033[0m [file] [options] -o [gen]
\\033[1mOptions:\\033[0m
{TAB}\\033[1m-v\\033[0m        show version information
{TAB}\\033[1m-c\\033[0m        debug mode (print AST)
{TAB}\\033[1m-s\\033[0m        generate cruntime to stdout
{TAB}\\033[1m-o\\033[0m        output file
{TAB}\\033[1m-vd\\033[0m       show version number only
{TAB}\\033[1m-p [path]\\033[0m  set libclang path (e.g., '-p /usr/lib/llvm-14/lib/libclang.so')
{TAB}\\033[1m-u\\033[0m        open this usage information
{TAB}\\033[1;31m-h\\033[0m\\033[31m        hardcore mode (wraps in xpcall for error catching)\033[0m
{TAB}\\033[1m[flags]\\033[0m     pass-through flags to gcc/g++ (e.g., '-Iinclude')\"\"\")
    sys.exit()
def version():
    print("\\033[1;34m" + "copyright:" + "\\033[0m" + " roblox-py " + "\\033[1m" + VERSION + "\\033[0m" + " licensed under the MIT License by " + "\\033[1m" + "@AsynchronousAI" + "\\033[0m (Luau Version)") # Updated version output to mention Luau
    sys.exit(0)
def main():
    # read args
    args = sys.argv[1:]
    flags = []
    inputf = None
    outputf = None
    check_ast = False

    lookForOutput = False
    skip = False
    hardcore = False

    if isconfig("lclang"):
        Config.set_library_file(isconfig("lclang"))

    flags = []

    for i, arg in enumerate(args):
        if arg.startswith("-W"):
            flags.append(arg)
        elif arg == "-o":
            lookForOutput = True
        elif arg == "-c":
            check_ast = True
        elif arg == "-h":
            hardcore = True
        elif arg == "-p":
            try:
                Config.set_library_file(args[i+1])
                config("lclang", args[i+1])
                skip = True
            except IndexError:
                config("lclang", None)
                error("no path specified after '-p'")
                sys.exit(1)
            except clang.LibclangError:
                config("lclang", None)
                error(f"Invalid libclang path: '{args[i+1]}'. Please provide a valid path to libclang. (e.g., '-p /usr/lib/llvm-14/lib/libclang.so')")
                sys.exit(1)

        elif arg == "-v":
            version()
        elif arg == "-vd":
            print(VERSION)
            sys.exit(0)
        elif arg == "-u":
            usage()
        elif arg == "-s":
            print(crun.cruntime)
            sys.exit(0)
        elif arg.startswith("-"):
            flags.append(arg)
        elif (inputf is None) and "-s" not in args:
            inputf = arg
        elif lookForOutput:
            outputf = arg
            lookForOutput = False
        elif skip:
            skip = False
        else:
            error("too many arguments")
            sys.exit(1)

    if (inputf is None) and "-s" not in args:
        usage()
        sys.exit(1)

    if outputf is None and "-s" not in args:
        error("no output file specified, use '-o [output file]'")
        sys.exit(1)

    isC = None
    if inputf and inputf.endswith(".c"):
        isC = True
    elif inputf and (inputf.endswith(".cpp") or inputf.endswith(".cxx") or inputf.endswith(".cc") or inputf.endswith(".C")):
        isC = False
    elif "-s" in args:
        pass # for -s, no input file type needed
    else:
        error("input file must end with '.c', '.cpp', '.cxx', '.cc', or '.C'")
        sys.exit(1)

    if inputf:
        parsed = get_ast(inputf, isC, check_ast, flags)
        if check_ast:
            print_ast(parsed)
        Engine = NodeVisitor(isC)
        Engine.visit(parsed)
        Enginecode = Engine.gen()
        header_code = gen_header_code(Enginecode)

        with open(outputf, "w") as f:
            code = (HEADER + header_code + Enginecode)
            if hardcore:
                code = "xpcall(function() -- hardcore mode\\n" + code + "\\nend, function(err) -- hardcore mode\\n\\terror('Segmentation fault: 11: ' .. err) -- hardcore mode\\nend) -- hardcore mode"

            f.write(code)
        info(f"Successfully compiled '{inputf}' to '{outputf}' (Luau)") # Updated info message to mention Luau
    elif "-s" in args:
        pass # cruntime already printed to stdout
    else:
        error("No input file and not generating cruntime (-s). Something went wrong.")


if __name__ == "__main__":
    main()
"""

# Create files
with open("crun.py", "w") as f:
    f.write(cruntime)
with open("rbx.h", "w") as f:
    f.write(rbx_h)
with open("rbxc.py", "w") as f:
    f.write(rbxc_py)
os.rename("crun.py", "crun.luau") # Renamed crun.py to crun.luau

print("Files 'crun.luau', 'rbx.h', and 'rbxc.py' created successfully.") # Updated output message to reflect .luau
print("rbxc.py has been updated to target Luau (Roblox Lua) and supports C23 and C++26 standards.")
print("To run rbxc.py, you might need to make it executable: 'chmod +x rbxc.py'")
print("\n**Note on Luau (Roblox Lua) and C23/C++26 Support:**")
print("This version of rbxc.py targets Luau, Roblox's Lua dialect. The 'cruntime.luau' file is now in Luau.") # Updated note to mention Luau
print("Basic Luau syntax and Roblox environment compatibility are improved.")
print("Type annotations have been added to 'cruntime.luau' for better Luau strict mode support (using --!strict).") # Added note about type annotations in cruntime
print("While rbxc.py attempts to parse C23 and C++26 code, full support for all new features is still not guaranteed.")
print("The compiler's code generation and runtime environment might not yet implement all latest language features.")
print("Please test thoroughly in the Roblox environment.")
