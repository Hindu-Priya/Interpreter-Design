"""
Calculator interpreter
  - A tree-walk interpreter for the calc language

Semantics of the Calc Language
1.) Math operations: +, -, *, /, ^ (**) with their usual meaning.
    Use conventional type widening and type rules.
       int _ int => int
       int _ float => float
       float _ int => float
       float _ float => float

2.) Input and Output
    - Input: input var
      Print out "var=" and read input from the user

      input x     =>    x=

    - Output: Any statement other than input or assignment statements
      will print their value to the screen.

      x=2
      x+3      =>  5

3.) Variable Rules
    - A variable must be initialized before it is read.
    - A variable can change its value (mutable)

Runtime Errors
  - Undefined variable
  - Division by zero
"""
import parser
from LEXER import Lexer, Token
from parser import Op, Parser
from enum import Enum, auto
from collections import ChainMap
import sys

# symbol table
class SymbolType:
    FUNCTION=auto()
    VARIABLE=auto()
    OBJECT = auto()
    CLASS = auto()

class SymbolEntry:
    def __init__(self, sym_type, sym_value):
        """
        Store type type and value
        """
        self.sym_type = sym_type
        self.sym_value = sym_value


class SymbolTable:
    def __init__(self, parent=None):
        """
        Create a nested symbol table.
        If parent is None, this is the topmost table.
        """
        self.tab = ChainMap()
        if parent is not None:
            self.tab = ChainMap(self.tab, parent.tab)


    def insert(self, name, value):
        """
        Insert into the most local map.
        """
        self.tab[name] = value


    def lookup(self, name):
        """
        Return the mapped object. If name is not present,
        return None
        """
        if name not in self.tab:
            return None
        return self.tab[name]


def assign(env, name, value):
    """
    Assign value to name, creating the variable if it is new.
    """

    # check to see if we have a symbol
    v = env.lookup(name)
    if v is None:
        # if it is new, insert into the symbol table.
        print("variable is not declared: " + name)
        sys.exit(-2)
    else:
        # set the value of the existing variable
        v.sym_value = value


def eval_tree(t, env):
    """
    Evaluate the parse tree t and return its result.
    """
    if t.op == Op.PROGRAM:
        return eval_program(t, env)
    elif t.op == Op.CLASS:
        return eval_class_decl(t, env)
    elif t.op == Op.BEGIN:
        return eval_begin(t, env)
    elif t.op == Op.WHILE:
        return eval_while(t, env)
    elif t.op == Op.ASSIGN:
        return eval_assign(t, env)
    elif t.op == Op.ADD:
        return eval_add(t, env)
    elif t.op == Op.SUB:
        return eval_sub(t, env)
    elif t.op == Op.MUL:
        return eval_mul(t, env)
    elif t.op == Op.DIV:
        return eval_div(t, env)
    elif t.op == Op.POW:
        return eval_pow(t, env)
    elif t.op == Op.NEG:
        return eval_neg(t, env)
    elif t.op == Op.INPUT:
        return eval_input(t, env)
    elif t.op == Op.VAL:
        return eval_val(t, env)
    elif t.op == Op.PRINT:
        return eval_print(t, env)
    elif t.op == Op.IF:
        return eval_if(t, env)
    elif t.op == Op.LESSTHAN:
        return eval_lt(t, env)
    elif t.op == Op.GREATERTHAN:
        return eval_gt(t, env)
    elif t.op == Op.LTEQUAL:
        return eval_lte(t, env)
    elif t.op == Op.NUMBER:
        return eval_number_decl(t, env)
    elif t.op == Op.CALL:
        return eval_call(t, env)
    elif t.op == Op.FUNCTION_DECLARATION:
        return eval_fundef(t, env)
    elif t.op == Op.OBJECT_DECLARATION:
        return eval_object_decl(t, env)
    elif t.op == Op.RETURN:
        return eval_return(t, env)
    elif t.op == Op.READ:
        return eval_read(t, env)

def eval_program(t, env):
    """
    Evaluate a program. Return the result of the last statement.
    """
    fun_result = None

    for c in t.children:
        # c is a statement, capture its result
        result = eval_tree(c, env)

        # check to see if we print
        if result is not None:
            fun_result = result

    return fun_result

def eval_begin(t, env):
    for c in t.children:
        if c.op == Op.RETURN:
            return eval_tree(c, env)
        eval_tree(c, env)

def eval_while(t, env):
    # evaluate the condition
    while eval_tree(t.children[0], env):
        eval_tree(t.children[1], env)

def eval_return(t, env):
    return eval_tree(t.children[0], env)

def eval_read(t, env):


    # take input from terminal
    val = int(input())
    name = t.children[0].token.lexeme

    if len(t.children[0].children) != 0:
        # array read
        index = eval_tree(t.children[0].children[0], env)
        assign_array(env, name, index, val)
    else:
        assign(env, name, val)

def eval_number_decl(t, env):
    name = t.children[0].token.lexeme
    v = env.lookup(name)
    if v is None:
        # if it is new, insert into the symbol table.
        # check if it is an array
        if len(t.children[0].children) != 0:
            arrSize = t.children[0].children[0].token.value
            # initialize an array of size <arrSize>
            array = [0 for i in range(arrSize)]
            env.insert(name, SymbolEntry(SymbolType.VARIABLE, array))
        else:
            env.insert(name, SymbolEntry(SymbolType.VARIABLE, 0))

    else:
        # set the value of the existing variable
        print("do not declare variable twice")
        sys.exit(-2)

def eval_assign(t, env):
    """
    Evaluate an assignment.
    """
    name = t.children[0].token.lexeme
    val = eval_tree(t.children[1], env)

    if len(t.children[0].children) != 0:
        index = eval_tree(t.children[0].children[0], env)
        assign_array(env, name, index, val)
    else:
        assign(env, name, val)

def assign_array(env, name, index, val):
    """
        Assign value to name, creating the variable if it is new.
        """

    # check to see if we have a symbol
    v = env.lookup(name)
    if v is None:
        # if it is new, insert into the symbol table.
        print("variable is not declared: " + name)
        sys.exit(-2)
    else:
        # set the value of the existing variable
        v.sym_value[index] = val

def eval_add(t, env):
    """
    Evaluate Addition
    """
    # evaluate the operands
    l = eval_tree(t.children[0], env)
    r = eval_tree(t.children[1], env)

    return l+r

def eval_sub(t, env):
    """
    Evaluate Subtraction
    """
    # evaluate the operands
    l = eval_tree(t.children[0], env)
    r = eval_tree(t.children[1], env)

    return l-r

def eval_mul(t, env):
    """
    Evaluate Multiplication
    """
    # evaluate the operands
    l = eval_tree(t.children[0], env)
    r = eval_tree(t.children[1], env)

    return l*r


def eval_div(t, env):
    """
    Evaluate Division
    """
    # evaluate the operands
    l = eval_tree(t.children[0], env)
    r = eval_tree(t.children[1], env)

    if r == 0:
        print(f"Division by Zero on line {t.token.line}")
        sys.exit(-2)

    return l/r


def eval_pow(t, env):
    """
    Evaluate Exponents
    """
    # evaluate the operands
    l = eval_tree(t.children[0], env)
    r = eval_tree(t.children[1], env)

    return l**r


def eval_neg(t, env):
    """
    Evaluate negation.
    """
    return - eval_tree(t.children[0], env)


def eval_input(t, env):
    """
    Evaluate input
    """
    global var

    # get the variable name
    name = t.children[0].token.lexeme

    # prompt for the value
    s = input(f"{name}=")

    # find our type
    if s.find('.'):
        assign(env, name, float(s))
    else:
        assign(env, name, int(s))


def eval_val(t, env):
    """
    Evaluate atomic value
    """
    if t.token.token == Token.REF:
        name = t.token.lexeme
        sym = env.lookup(name)
        if sym is None:
            print(f"Undefined variable at line {t.token.line}.")
            sys.exit(-1)

        if len(t.children) != 0:
            # it is an array
            index = eval_tree(t.children[0], env)
            return sym.sym_value[index]
        return sym.sym_value

    return t.token.value


def eval_print(t, env):
    print(eval_tree(t.children[0], env))


def eval_if(t, env):
    # evaluate the condition
    c = eval_tree(t.children[0], env)

    if c:
        eval_tree(t.children[1], env)


def eval_lt(t, env):
    left = eval_tree(t.children[0], env)
    right = eval_tree(t.children[1], env)
    return left < right

def eval_gt(t, env):
    left = eval_tree(t.children[0], env)
    right = eval_tree(t.children[1], env)
    return left > right

def eval_lte(t, env):
    left = eval_tree(t.children[0], env)
    right = eval_tree(t.children[1], env)
    return left <= right

def eval_eqto(t, env):
    left = eval_tree(t.children[0], env)
    right = eval_tree(t.children[1], env)
    return left == right


# def eval_call(t, env):
#     # Recall the children:
#     #    ref arg1, arg2, .... argn
#
#     # retrieve the function
#     name = t.token.lexeme
#     fun = env.lookup(name)
#     if fun is None:
#         print(f"Call to undefined function {name} on line {t.token.line}")
#         sys.exit(-1)
#     elif fun.sym_type != SymbolType.FUNCTION:
#         print(f"Call to non-function {name} on line {t.token.line}")
#         sys.exit(-1)
#
#     # extract the function part of fun
#     fun = fun.sym_value
#
#     # now fun has: p1, p2, ... pn progam
#     if len(t.children) != len(fun.children) - 3:
#         print(f"Incorrect number of arguments for {len(t.children)} on line {len(fun.children)}")
#
#     # create a new reference environment
#     local_env = SymbolTable(env)
#
#     # parameters are always local
#     for i in range(len(t.children)):
#         value = eval_tree(t.children[i], env)
#         name = fun.children[i+2].children[0].token.lexeme
#         local_env.insert(name, SymbolEntry(SymbolType.VARIABLE, value))
#
#     # call the function
#     return eval_tree(fun.children[-1], local_env)

def eval_call(t, env):

    obj_name = t.token.lexeme
    obj = env.lookup(obj_name)
    if obj is None:
        print(f"Call to undefined object {obj_name} on line {t.token.line}")
        sys.exit(-1)

    elif obj.sym_type != SymbolType.OBJECT:
        print(f"Call to non-object {obj_name} on line {t.token.line}")
        sys.exit(-1)

    obj_env = obj.sym_value


    entity = t.children[0].token.lexeme
    if len(t.children) > 1:
        #called entity is a function.. get function and execute
        fun = obj_env.lookup(entity)
        if fun is None:
            print(f"Call to undefined function  {entity} on line {t.token.line}")
            sys.exit(-1)
        elif fun.sym_type != SymbolType.FUNCTION:
            print(f"Call to non-function {entity} on line {t.token.line}")
            sys.exit(-1)

        if len(fun.sym_value.children) - 4 != len(t.children) - 2:
            print(f"no of parameters are not matching the function signature on line {t.token.line}")
            sys.exit(-1)

        if fun.sym_value.children[0].op == Op.NOACCESS:
            print(f"could not find the method {entity} on line {t.token.line}")
            sys.exit(-1)

        # create a new reference environment
        local_env = SymbolTable(obj_env)

        # parameters are always local
        for i in range(len(t.children)):
            #update local env with parameters and execute the function
            if t.children[i+1].op == Op.CLASS:
                break
            value = eval_tree(t.children[i + 1], env)
            name = fun.sym_value.children[i + 3].children[0].token.lexeme
            local_env.insert(name, SymbolEntry(SymbolType.VARIABLE, value))

        # call the function
        return eval_tree(fun.sym_value.children[-1], local_env)

def eval_fundef(t, env):
    name = t.children[2].token.lexeme
    env.insert(name, SymbolEntry(SymbolType.FUNCTION, t))

def eval_class_decl(t, env):
    name = t.token.lexeme
    #look if the function is already present
    if env.lookup(name) is None:
        env.insert(name, SymbolEntry(SymbolType.CLASS, t))


def eval_object_decl(t, env):
    #get class definition and create environment
    className = t.children[0].token.lexeme
    cls = env.lookup(className)

    if cls is None:
        print(f"undefined class {className} on line {t.token.line}")
        sys.exit(-1)
    elif cls.sym_type != SymbolType.CLASS:
        print(f"undefined class {className} on line {t.token.line}")
        sys.exit(-1)

    class_env = SymbolTable(env)
    class_tree = cls.sym_value

    #if this is an inherited class
    if class_tree.children[0].op == Op.CHILDOF:
        #add variables from parent class and functions as well
        parent = class_tree.children[0].token.lexeme
        parent_class = env.lookup(parent)
        if parent_class is None:
            print(f"undefined class {parent} on line {t.token.line}")
            sys.exit(-1)
        elif cls.sym_type != SymbolType.CLASS:
            print(f"undefined class {parent} on line {t.token.line}")
            sys.exit(-1)

        parent_class_tree = parent_class.sym_value
        for j in range(len(parent_class_tree.children)):
            if parent_class_tree.children[j].op == Op.FUNCTION_DECLARATION:
                eval_fundef(parent_class_tree.children[j], class_env)



    #initialize all variables and store all functions
    for i in range(len(class_tree.children)):
        if class_tree.children[i].op == Op.FUNCTION_DECLARATION:
            eval_fundef(class_tree.children[i], class_env)

    name = t.children[1].token.lexeme
    env.insert(name, SymbolEntry(SymbolType.OBJECT, class_env))

if __name__=="__main__":
    # read from either stdin or a file

    l = Lexer(open(sys.argv[1]))
    p = Parser(l)
    t = p.parse()
    eval_tree(t, SymbolTable())
