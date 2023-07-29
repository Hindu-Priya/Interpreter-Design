"""
This module contains the parser for the calc language.
This is a recursive descent parser for an LL(1) Grammar
"""

import sys
from LEXER import Lexer, Token
from enum import Enum, auto


# 1.) Create the generic parser.
#     - lexer reference
#     - 1 token 
#     - utility functions 
#        has(tok) - true if tok is the current token
#        must_be(tok) - like has but prints an error if it does not match
#                       It will also kill the program
#        consume() - Advance the parser to the next token
# 2.) Create parse_ functions from the BNF rules

# To build a parse tree
# 1.) Create an enumeration for all of our operations.
# 2.) Create an object to represent parse trees.
# 3.) Make each parse_* function return a parse tree.
# 4.) parse function returns the parse_program

class Op(Enum):
    PROGRAM = auto()
    CLASS = auto()
    CHILDOF = auto()
    NOACCESS = auto()
    ASSIGN = auto()
    RETURN = auto()
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    POW = auto()
    NEG = auto()
    INPUT = auto()
    VARIABLE_DECLARATION= auto()
    FUNCTION_DECLARATION = auto()
    OBJECT_DECLARATION = auto()
    VAL = auto()
    PRINT = auto()
    NUMBER = auto()
    CHARACTER = auto()
    VOID = auto()
    BEGIN = auto()
    PROC = auto()
    WHILE = auto()
    READ = auto()
    EQUALTO = auto()
    IF = auto()
    ELSE = auto()
    APPROXIMATELYEQUAL = auto()
    SWAP_OP = auto()
    LESSTHAN = auto()
    LTEQUAL = auto()
    GREATERTHAN = auto()
    GREATERTHAN_OR_EQUAL = auto()
    TYPE = auto()
    CALL = auto()

class ParseTree:
    def __init__(self, op=None, children=[], token=None):
        self.op = op
        self.children = list(children)
        self.token = token

    def print(self, level=0):
        n = int(len(self.children)/2)


        # print the right half
        for child in self.children[n:]:
            child.print(level = level+1)

        # print the node
        print(level, end=' ')

        if self.op == Op.VAL:
            print(self.token.lexeme)
        else:
            print(self.op.name)

        # print the left half
        for child in self.children[0:n]:
            child.print(level = level+1)


    def insert_binary_left_leaf(self, t):
        """
        Assume that all operations we will encounter are binary.
          (Use only with binary opeartors)
        """
        if len(self.children)<2:
            self.children.insert(0, t)
        else:
            self.children[0].insert_binary_left_leaf(t)

class Parser:
    def __init__(self, lexer):
        # parser state
        self.lexer = lexer

        # grab that first token
        self.consume()


    def consume(self):
        """
        Advance the parser to the next token.
        """
        self.token = self.lexer.next()
        # can be removed
        print(self.token)


    def has(self, t):
        """
        Returns true if the parser has token t in look-ahead.
        False otherwise.
        """
        return t == self.token.token


    def must_be(self, t):
        """
        Returns true if self.has(t). Otherwise, print an error
        message and abort the program.
        """
        if self.has(t):
       
            return True

        # report error and exit
        print(f"Error at line {self.token.line} and column {self.token.col} unexpected token: {self.token.token.name} {self.token.lexeme}")
        sys.exit(-1)


    def parse(self):
        """
        Attempt to parse.
        """
        return self.parse_program()


    ########## parser rules follow ##################
    def parse_program(self):
        """
         < program >     ::= < var-declist > < fun-declist > < block >
                    | "PROC" <fun-declist> < block >  
                    | < block >        
                    | <type> <fun-declist > < block >  # manageable with if else
     
        """
        tree = ParseTree(Op.PROGRAM, token=self.token)

        while not self.has(Token.EOF):

            if self.has(Token.NUMBER) or self.has(Token.CHARACTER):
                tp = ParseTree(Op.TYPE, token=self.token)
                self.consume()
                self.must_be(Token.REF)
                name = ParseTree(Op.VAL, token=self.token)
                self.consume()
                if self.has(Token.LPAREN):
                    f = ParseTree(Op.FUNCTION_DECLARATION, token=self.token)
                    f.children.append(tp)
                    f.children.append(name)
                    self.consume()
                    if self.has(Token.RPAREN):
                        self.consume()
                        b = self.parse_block()
                        f.children.append(b)
                        tree.children.append(f)
                        f2 = False
                        f2 = self.parse_fun_declist_p()
                        if f2:
                            tree.children.append(f2)

                    elif self.has(Token.NUMBER) or self.has(Token.CHARACTER):
                        x = self.parse_param_list()
                        self.must_be(Token.RPAREN)
                        b = self.parse_block()
                        f.children.append(b)
                        tree.children.append(f)
                        f2 = False
                        f2 = self.parse_fun_declist_p()
                        if f2:
                            tree.children.append(f2)
                else:
                    if self.has(Token.LSQUAREBRACKET):
                        self.consume()
                        self.must_be(Token.RSQUAREBRACKET)
                        self.consume()
                        t = False
                        t = self.parse_var_decl1()
                        if t:
                            tree.children.append(t)
            elif self.has(Token.BEGIN):
                tree.children.append(self.parse_block())
            elif self.must_be(Token.BLUEPRINT):
                self.parse_class_declist(tree)
                tree.children.append(self.parse_block())
            #clear below statement
            elif self.must_be(Token.PROC):
                # f = ParseTree(Op.FUNCTION_DECLARATION, token=self.token)
                self.parse_fun_declist(tree)
                tree.children.append(self.parse_block())
                # f.children.append(t)
                # e = self.parse_block()
                # f.children.append(e)
                # tree.children.append(f)
        
        # print([n.op for n in tree.children])
        return tree

    def parse_class_declist(self, program):
        """
        < class-declist > ::= < class >< class-declist > | " "
        """

        cls = self.parse_class()
        program.children.append(cls)

        if self.has(Token.BLUEPRINT):
            self.parse_class_declist(program)
        return None

    def parse_class(self):
        if self.has(Token.BLUEPRINT):
            self.consume()

            self.must_be(Token.REF)
            cls = ParseTree(Op.CLASS, token= self.token)
            self.consume()

            if self.has(Token.CHILDOF):
                self.consume()
                self.must_be(Token.REF)

                cls.children.append(ParseTree(Op.CHILDOF, token= self.token))
                self.consume()

            self.parse_class_block(cls)
            return cls

    def parse_class_block(self, cls):
        if self.must_be(Token.BEGIN):
            self.consume()

            self.parse_class_block1(cls)

        return None

    def parse_class_block1(self, cls):
        if self.has(Token.PROC) or self.has(Token.NOACCESS):
            self.parse_fun_declist(cls)
        elif self.has(Token.NUMBER) or self.has(Token.CHARACTER):
            self.parse_var_declist(cls)
            self.parse_fun_declist(cls)

        self.must_be(Token.END)
        self.consume()
        return None



    def parse_fun_declist(self, program):

        """
        < fun-declist > ::= < fun > <fun-declist'>
        """
        fun = self.parse_fun()
        program.children.append(fun)

        self.parse_fun_declist_p(program)
        return None

    def parse_fun_declist_p(self, program):

        """
        <fun-declist'> ::= < fun > < fun-declist' > 
                            |""

        """
        if self.has(Token.NOACCESS) or self.has(Token.PROC):
            self.parse_fun_declist(program)
            # t = self.parse_fun()
            # e = False
            # e = self.parse_fun_declist_p()
            # if e:
            #     e.insert_binary_left_leaf(t)
            #     result = e
            # else:
            #     result = t
        else:
            return None

    def parse_fun(self):
        """

        < fun >         ::=  <fun_type> REF "(" < fun1 >  
        """
        if self.has(Token.NOACCESS) or self.must_be(Token.PROC):

            if self.has(Token.PROC):
                access = ParseTree(Op.PROC, token= self.token )
            elif self.has(Token.NOACCESS):
                access = ParseTree(Op.NOACCESS, token= self.token )

            self.consume()

        f = ParseTree(Op.FUNCTION_DECLARATION, token=self.token)
        f.children.append(access)

        if self.has(Token.NUMBER):
            fun_type = ParseTree(Op.NUMBER, token= self.token)
        elif self.has(Token.CHARACTER):
            fun_type = ParseTree(Op.CHARACTER, token= self.token)
        elif self.must_be(Token.VOID):
            fun_type = ParseTree(Op.VOID, token= self.token)

        f.children.append(fun_type)
        self.consume()

        self.must_be(Token.REF)
        name = ParseTree(Op.VAL, token=self.token)
        self.consume()
        f.children.append(name)
        self.must_be(Token.LPAREN)
        # consuming LPAREN
        self.consume()
        self.parse_fun1(f)

        return f


    def parse_fun1(self, function):

        """
        < fun1 >        ::= < param-list > ")" < block >
                        | ")" < block >
        """
       
        if self.has(Token.NUMBER) or self.has(Token.CHARACTER):
            self.parse_param_list(function)
            # consuming Right Parenthesis
            self.consume()
            e = self.parse_block()    
            function.children.append(e)
        elif self.must_be(Token.RPAREN):
            self.consume()
            result = self.parse_block()
            function.children.append(result)

        return None

    def parse_param_list(self, function):
        """     
        #< fun-type >    ::= "PROC"
                        | < type >
        < param-list >  ::= < param-decl > < param-list' >
        """
        t = self.parse_param_decl()
        function.children.append(t)
        self.parse_param_list_p(function)

        return None

    def parse_param_list_p(self, function):

        """
        < param-list' > ::=  "," < param-decl > < param-list' >
                        | ""
        """
        if self.has(Token.COMMA):
            self.consume()
            t = self.parse_param_decl()
            function.children.append(t)
            self.parse_param_list_p(function)

        return None

    def parse_param_decl(self):
        """
            < param-decl >  ::= < type > < param-decl1 >
        """
        t = self.parse_type()
        e = False
        e = self.parse_param_decl1()
        if e:
            t.children.append(e)

        return t

    def parse_param_decl1(self):
        """
        < param-decl1 > ::= "REF"
                        | "[]" "REF"
        """
        if self.has(Token.REF):
            result = ParseTree(Op.VAL, token=self.token)
            self.consume()
        elif self.must_be(Token.LSQUAREBRACKET):
            self.consume()
            self.must_be(Token.RSQUAREBRACKET)
            self.consume()
            self.must_be(Token.REF)
            result = ParseTree(Op.VAL, token=self.token)
            result.children.append(ParseTree(Op.VAL, token= self.token))
            self.consume()
        return result

    def parse_block(self):
        """
        < block >       ::= "BEGIN" < block1>   
        """
        if self.must_be(Token.BEGIN):
            tree = ParseTree(Op.BEGIN, token= self.token)
            self.consume()
            self.parse_block1(tree)
            # if t:
            #     return t
      
        return tree

    def parse_block1(self, begin):
        """
        < block1>       ::= < var-declist > < stmnt-list > "END"
                        | < stmnt-list > "END"
            self.consume()
            if t:
                if e:
                    e.insert_binary_left_leaf(t)
                    result = e

                else:
                    result = t
        """
        if self.has(Token.NUMBER) or self.has(Token.CHARACTER) or self.has(Token.OBJECTOF):
            self.parse_var_declist(begin)
            self.parse_stmnt_list(begin)
            self.must_be(Token.END)
            self.consume()

        else: 
            self.parse_stmnt_list(begin)
            self.must_be(Token.END)
            self.consume()
        return None

    def parse_var_declist(self, begin):
        """

        < var-declist > ::= < var-decl > < var-declist1 >      
        """
        if self.has(Token.NUMBER) or self.has(Token.CHARACTER) or self.has(Token.OBJECTOF):
            decl = self.parse_var_decl()
            begin.children.append(decl)
            self.parse_var_declist1(begin)

    def parse_var_declist1(self, begin):
        """
        < var-declist1 > ::= < var-declist > 
                            |""
        """
        if self.has(Token.NUMBER) or self.has(Token.CHARACTER) or self.has(Token.OBJECTOF):
            self.parse_var_declist(begin)

    def parse_var_decl(self):
        """
        < var-decl >    ::= < type > "REF"< var-decl1 > 
        """
        if self.has(Token.OBJECTOF):
            return self.parse_object_decl()

        type = self.parse_type()

        self.must_be(Token.REF)
        inner_left = ParseTree(Op.VAL, token=self.token)
        self.consume()
        if self.has(Token.LSQUAREBRACKET):
            self.consume()
            self.must_be(Token.INTLIT)
            arrSize = self.token
            self.consume()
            self.must_be(Token.RSQUAREBRACKET)
            inner_left.children.append(ParseTree(Op.VAL, token=arrSize))
            self.consume()

        type.children.append(inner_left)
        # t = False
        # if self.has(Token.LSQUAREBRACKET):
        #     t = self.parse_var_decl1()
        #     if t:
        #         t.insert_binary_left_leaf(inner_left)
        #         t.insert_binary_left_leaf(left)
        #         result = t
        # else:
        #     inner_left.insert_binary_left_leaf(left)
        #     result = inner_left

        return type
           

    def parse_var_decl1(self):
        """
            
        < var-decl1 >   ::=  "[" < bounds > "]"
                        
        """
        if self.must_be(Token.LSQUAREBRACKET):
            self.consume()
            result = self.parse_bounds()
            self.consume()
        return result

    def parse_bounds(self):
        """
        < bounds >      ::=  "INTLIT" <  < bounds' >
        """
        self.must_be(Token.INTLIT)
        tree = ParseTree(Op.VAL, token= self.token)
        self.consume()
        result = False
        result = self.parse_bounds_p()
        if result:
            result.insert_binary_left_leaf(tree)
            tree = result
        return tree

    def parse_bounds_p(self):
        """
        < bounds' >     ::=  "," "INTLIT" < bounds' >
                        | ""
        """
        if self.has(Token.COMMA):
            self.consume()
            self.must_be(Token.INTLIT)
            tree = ParseTree(Op.VAL, token= self.token)
            self.consume()
            result = False
            result = self.parse_bounds_p()
            if result:
                result.insert_binary_left_leaf(tree)
                tree = result
        else:
            return False

        return tree

    def parse_type(self):
        """
        < type >        ::= "NUMBER"
                        | "CHARACTER"
        """
        if self.must_be(Token.NUMBER) or self.has(Token.CHARACTER):
            if self.has(Token.NUMBER):
                tree = ParseTree(Op.NUMBER, token= self.token)
                self.consume()
            elif self.has(Token.CHARACTER):
                tree = ParseTree(Op.CHARACTER, token= self.token)
                self.consume()
            return tree

    def parse_stmnt_list(self, begin):
        """
        < stmnt-list >  ::= < stmnt >< stmnt-list1 >                 
        """
        """
        < exponent >    ::= "(" < expr > ")"
                    | < ref >
                    | < literal >
                    | < call >
        """
        t = self.parse_stmnt()
        begin.children.append(t)

        self.parse_stmnt_list1(begin)

        return None


    def parse_stmnt_list1(self, begin):
        """
        < stmnt-list1 > ::= < stmnt-list >
                        |""
        """
        if self.has(Token.END):
             return None
        else:
            self.parse_stmnt_list(begin)
            return None

    def parse_stmnt(self):
        """
        < stmnt >       ::= < assign-or-swap > 
                        | < branch > 
                        | < loop > 
                        | < expr >
                        | < print >
                        | < read >
                        | < return >
        """
        if self.has(Token.IF):
            result = self.parse_branch()
        elif self.has(Token.WHILE):
            result = self.parse_loop()
        elif self.has(Token.PRINT):
            result = self.parse_print()
        elif self.has(Token.READ):
            result = self.parse_read()
        elif self.has(Token.RETURN):
            result = self.parse_return()
        elif self.has(Token.REF):
            result = self.parse_assign_Or_swap()
        else:
            result = self.parse_expr()
        return result

    def parse_object_decl(self):
        if self.must_be(Token.OBJECTOF):
            tree = ParseTree(Op.OBJECT_DECLARATION, token= self.token)
            self.consume()
            self.must_be(Token.REF)
            tree.children.append(ParseTree(Op.VAL, token= self.token))
            self.consume()
            self.must_be(Token.REF)
            tree.children.append(ParseTree(Op.VAL, token= self.token))
            self.consume()

            return tree

    def parse_assign(self):
        """
        < assign >      ::= ":=" < expr >
        """
        if self.must_be(Token.ASSIGN):
            tree = ParseTree(Op.ASSIGN, token=self.token)
            self.consume()
            t = self.parse_expr()
            tree.children.append(t)
            return tree

    def parse_swap(self):
        """
        < swap >        ::= ":=:" < ref >
        """
        if self.must_be(Token.SWAP):
            tree = ParseTree(Op.SWAP_OP, token=self.token)
            self.consume()

            tree.children.append(self.parse_ref())
            return tree

    def parse_a_Or_s(self):
        """
        < a-or-s>       ::= < assign > 
                        | < swap >
        """
        if self.has(Token.ASSIGN):
            result = self.parse_assign()
        elif self.must_be(Token.SWAP):
            result = self.parse_swap()
        return result

    def parse_assign_Or_swap(self):
        """
        <assign-or-swap> ::= < ref > < a-or-s >
        """
        t = False
        if self.has(Token.REF):
            f = self.parse_ref()
            if self.has(Token.ASSIGN) or self.has(Token.SWAP):
                t = self.parse_a_Or_s()
        if t:
            t.insert_binary_left_leaf(f)
            result = t
        else:
            result = f
        return result


    def parse_branch(self):
        """
        < branch >      ::= "IF" < condition > < block >< branch1 > 
        """
        if self.must_be(Token.IF):
            tree = ParseTree(Op.IF, token= self.token)
            self.consume()
            child1 = self.parse_condition()
            child2 = self.parse_block()
            tree.children.append(child1)
            tree.children.append(child2)
            child3 = self.parse_branch1()   
            if child3:
                tree.children.append(child3)

            return tree

    def parse_branch1(self):
        """               
        < branch1 >     ::= "ELSE" < block > 
                        |""
        """
        if self.has(Token.ELSE):
            tree = ParseTree(Op.ELSE, token= self.token)
            self.consume()
            tree.children.append(self.parse_block())
            return tree
        else:
            return False
        

    def parse_loop(self):
        """
        < loop >        ::= "WHILE" < condition > < block >
        """
        if self.must_be(Token.WHILE):
            tree = ParseTree(Op.WHILE, token=self.token)
            self.consume()
            t = self.parse_condition()
            e = self.parse_block()
            tree.children.append(t)
            tree.children.append(e)

            return tree

    def parse_condition(self):
        """
        < condition >   ::= < expr > < condition1 >
        """
        t = self.parse_expr()
        e = self.parse_condition1()
        if e:
            e.insert_binary_left_leaf(t)
            return e
        else:
            return t

    def parse_condition1(self):
        """
        < condition1 >  ::= "=" < expr > 
                        | "~=" < expr>
                        | "<" < expr >
                        | "<=" < expr >
                        | ">" < expr >
                        | ">=" < expr >
        """
        if self.has(Token.EQUAL):
            tree = ParseTree(Op.EQUALTO, token=self.token)
            self.consume()
            t = self.parse_expr()
        elif self.has(Token.APPROXEQUAL):
            tree = ParseTree(Op.APPROXIMATELYEQUAL, token=self.token)
            self.consume()
            t = self.parse_expr()
        elif self.has(Token.LESSTHAN):
            tree = ParseTree(Op.LESSTHAN, token=self.token)
            self.consume()
            t = self.parse_expr()
        elif self.has(Token.LTEQUAL):
            tree = ParseTree(Op.LTEQUAL, token=self.token)
            self.consume()
            t = self.parse_expr()
        elif self.has(Token.GREATERTHAN):
            tree = ParseTree(Op.GREATERTHAN, token=self.token)
            self.consume()
            t = self.parse_expr()
        elif self.must_be(Token.GTEQUAL):
            tree = ParseTree(Op.GREATERTHAN_OR_EQUAL, token=self.token)
            self.consume()
            t = self.parse_expr()

        tree.children.append(t)
        return tree

    def parse_expr(self):
        """
            < expr >        ::= < term > < expr' >
        """
        t = self.parse_term()
        e = False
        if self.has(Token.PLUS) or self.has(Token.MINUS):
           e = self.parse_expr_p()
        if e:
            e.insert_binary_left_leaf(t)
            return e
        else:
            return t

    def parse_expr_p(self):
        """
        < expr' >       ::= "+" < term > < expr' >
                        | "-" < term > < expr' > 
                        | ""
        """
        if self.has(Token.PLUS):
            tree = ParseTree(Op.ADD, token=self.token)
            self.consume()
            t = self.parse_term()
            e = self.parse_expr_p()
        elif self.has(Token.MINUS):
            tree = ParseTree(Op.SUB, token=self.token)
            self.consume()
            t = self.parse_term()
            e = self.parse_expr_p()
        else:
            return False
        tree.children.append(t)

        if e:
            e.insert_binary_left_leaf(tree)
            tree = e
        return tree
        
    def parse_term(self):
        """
        < term >        ::= < factor > <term'>
        """
        t = self.parse_factor()
        e = False
        if self.has(Token.TIMES) or self.has(Token.DIVIDE):
            e = self.parse_term_p()
        if e:
            e.insert_binary_left_leaf(t)
            result = e
        else:
            result = t
        return result

    def parse_term_p(self):
        """
        <term'>         ::=  "*" < factor > < term' >
                        | "/" < factor > < term' >
                        |""
        """
        result = False
        if self.has(Token.TIMES):
            result = ParseTree(Op.MUL, token= self.token)
            self.consume()
            t = self.parse_factor()
            e = self.parse_term_p()
        elif self.has(Token.DIVIDE):
            result = ParseTree(Op.DIV, token= self.token)
            self.consume()
            t = self.parse_factor()
            e = self.parse_term_p()

        if result is False:
            return result
        result.children.append(t)

        if e:
            e.insert_binary_left_leaf(result)
            result = e
        return result

    def parse_factor(self):
        """
        < factor >      ::= < exponent > < factor' >
        """
        t = self.parse_exponent()
        e = False
        if self.has(Token.EXPONENT):
            e = self.parse_factor_p()
        if e:
            e.insert_binary_left_leaf(t)
            result = e
        else:
            result = t

        return result

    def parse_factor_p(self):
        """
            < factor' >      ::=  "**" < factor >< factor' >
                        | ""
                        """
        e = False
        if self.has(Token.EXPONENT):
            result = ParseTree(Op.POW, token=self.token)
            self.consume()

            t = self.parse_factor()
            e = self.parse_factor_p()

        result.children.append(t)
        if e:
            e.insert_binary_left_leaf(result)
            result = e

        return result
    
    def parse_exponent(self):
        """

        < exponent >    ::= "(" < expr > ")"
                    | < ref >
                    | < literal >
                    | < call >
        """
        if self.has(Token.LPAREN):
            self.consume()
            result = self.parse_expr()
            self.must_be(Token.RPAREN)
            self.consume()
        # elif self.has(Token.REF) and self.has(Token.LPAREN):
        #     self.parse_call()
        elif self.has(Token.INTLIT) or self.has(Token.FLOATLIT) or self.has(Token.CHARLIT) or self.has(Token.STRING):
            result = self.parse_literal()
        elif self.must_be(Token.REF):
            result = self.parse_ref()

        return result
 

    def parse_print(self):
        """
        < print >       ::= "PRINT" < arg-list >
        """
        if self.must_be(Token.PRINT):
            result = ParseTree(Op.PRINT, token=self.token)
            self.consume()
            t = self.parse_arg_list()
            if t:
              result.children.append(t)
            
            return result

    def parse_arg_list(self):
        """
        < arg-list >    ::= < expr > < arg-list1 >
        """
        t = self.parse_expr()
        e = False
        if self.has(Token.COMMA):
            e = self.parse_arg_list1()
       
        if e:
            e.insert_binary_left_leaf(t)
            t = e
        return t

    def parse_arg_list1(self):
        """               

        < arg-list1 >   ::= "," < arg-list >
                        |""
        """
        if self.has(Token.COMMA):
            self.consume()
            return self.parse_arg_list()
        else:
            return False

    def parse_read(self):
        """                

        < read >        ::= "READ" < ref-list >
        """
        if self.must_be(Token.READ):
            result = ParseTree(Op.READ, token=self.token)
            self.consume()
            t = self.parse_ref_list()
            
            result.children.append(t)
            return result

    def parse_ref_list(self):
        """
        < ref-list >    ::= < ref > < ref-list1 >
        """
        t = self.parse_ref()
        e = False
        e = self.parse_ref_list1()
        if e:
            e.insert_binary_left_leaf(t)
            t = e
        return t

    def parse_ref_list1(self):
        """              
        < ref-list1 >   ::= "," < ref-list >
                        |""
        """
        if self.has(Token.COMMA):
            self.consume()
            return self.parse_ref_list()
        else:
            return False

    def parse_return(self):
        """

        < return >      ::= "RETURN" < return1 > 
        """
        if self.must_be(Token.RETURN):
            self.consume()
            ret = ParseTree(Op.RETURN, token= self.token)
            t = False
            t = self.parse_return1()     
            if t:
                ret.children.append(t)
                return ret
            else:
                return 

    def parse_return1(self):         
        """
        < return1 >     ::= < ref >
                        | < literal >
                        | ""

        """
        if self.has(Token.REF):
            t = self.parse_ref()
        elif self.has(Token.INTLIT) or self.has(Token.FLOATLIT) or self.has(Token.CHARLIT) or self.has(Token.STRING):
            t = self.parse_literal()
        else:
            return False
        return t


    def parse_ref(self):
        """
        < ref >         ::= "REF" < ref1 >
        """
        class_name = self.token
        if self.must_be(Token.REF):
            result = ParseTree(Op.VAL, token=self.token)
            self.consume()
        t =  False
        if self.has(Token.LSQUAREBRACKET):
            t = self.parse_ref1()
            result.children.append(t)
        #below is function call
        # elif self.has(Token.LPAREN):
        #     p = ParseTree(Op.CALL, token=class_name)
        #     self.consume()
        #     # parse expression again
        #     self.parse_params(p)
        #     self.must_be(Token.RPAREN)
        #     self.consume()
        #     return p
        elif self.has(Token.DOT):
            p = ParseTree(Op.CALL, token=class_name)
            self.consume()

            self.must_be(Token.REF)
            entity = self.token
            self.consume()
            p.children.append(ParseTree(Op.VAL, token= entity))
            #entity can be either a variable or a function
            if self.has(Token.LPAREN):
                #function call
                self.consume()
                self.parse_params(p)
                self.must_be(Token.RPAREN)
                self.consume()

                p.children.append(ParseTree(Op.CLASS, token= entity))
                return p
            else:
                return p


        return result

    def parse_params(self, call):
        if self.has(Token.REF):
            t = self.parse_ref()
            call.children.append(t)
            if self.has(Token.COMMA):
                self.consume()
                self.parse_params(call)


    def parse_ref1(self):
        """
        < ref1 >        ::= "[" < arg-list > "]"
                        | ""
        """
        if self.has(Token.LSQUAREBRACKET):
            self.consume()
            t = self.parse_arg_list()
            self.must_be(Token.RSQUAREBRACKET)
            self.consume()
        else:
            return False
        return t

    def parse_literal(self):
        """
        < literal >     ::= "INTLIT"
                        | "FLOATLIT"
                        | "CHARLIT"
                        | "STRING"

        """
        if self.has(Token.INTLIT) or self.has(Token.FLOATLIT) or self.has(Token.CHARLIT) or self.must_be(Token.STRING):
            result = ParseTree(Op.VAL, token=self.token)
            self.consume()
        return result

    def parse_call(self):
        """
        < call >        ::= "(" <call1>
        """
        if self.must_be(Token.LPAREN):
            self.consume()
            t = self.parse_call1()
        if t:
            return t 



    def parse_call1(self):
        """
        <call1>         ::= < arg-list > ")"
                        | ")"

        """
        if self.has(Token.RPAREN):
            self.consume()
        else:
            t = self.parse_arg_list()
            self.must_be(Token.RPAREN)
            self.consume()
            return t

if __name__ == "__main__":
    lexer = Lexer(open(sys.argv[1]))
    p = Parser(lexer)
    t = p.parse()
    t.print()
    
