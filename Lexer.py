"""
This file contains the lexer for the calc language.

What a lexer does:
    - Recognizes the regular portion of a grammar.
    - Reads input character by character and builds tokens.

How to a Construct a Lexer:
    1.) Construct an interface for the input. Scan character by character,
        keeping track of line and column number.

    2.) Create a function which can skip whitespace.

    3.) Examine our BNF, and find all the things that are regular.
         A->a  or A->aA
         A->a  or A->Aa

         - All terminals are automatically regular.
         - Which rules can be written as a regular grammar.

    4.) Create a representation for all the tokens.
         - Token: Broad category of strings (lexer's output)
         - Lexeme: Actual string that was matched to the token.
         - Value: Any lexeme that can be converted a value will have a value.
         - Line: Line number where the token begins
         - Col: Column number where the token begins

    5.) Create a function called "next" which advances the lexer to
        the next lexeme.

        5.1) Group tokens according to difficulty.
             Group 1 - Single characters which are not the prefix of
                       anything else. (+, /, =)

             Group 2 - Fixed width tokens which may include prefixes of
                       each other. (<, <=, >, >=...)

             Group 3 - Variable width tokens, may include some fixed-width
                       tokens which can be prefixes of wider tokens.
                       Numbers, Variable Names, Keywords
"""
import sys
from enum import Enum,auto
from collections import namedtuple

class Token(Enum):
    """
    Fun Lang tokens
    """
    PROC = auto()
    OBJECTOF = auto()
    VOID = auto()
    BLUEPRINT = auto()
    NOACCESS = auto()
    CHILDOF = auto()
    EQUAL = auto()
    PLUS = auto()
    MINUS = auto()
    TIMES = auto()
    DIVIDE = auto()
    EXPONENT = auto()
    INTLIT = auto()
    FLOATLIT = auto()
    CHARLIT = auto()
    INVALID = auto()
    BEGIN = auto()
    END = auto()
    LSQUAREBRACKET = auto()
    RSQUAREBRACKET = auto()
    COMMA = auto()
    IF = auto()
    ELSE = auto()
    WHILE = auto()
    APPROXEQUAL = auto()
    LESSTHAN = auto()
    LTEQUAL = auto()
    GREATERTHAN = auto()
    GTEQUAL = auto()
    ASSIGN = auto()
    SWAP = auto()
    LPAREN = auto()
    RPAREN = auto()
    DOT = auto()
    BACKSLASH = auto()
    SINGLEQUOT = auto()
    REF = auto()
    NUMBER = auto()
    CHARACTER = auto()
    UNDERSCORE = auto()
    PRINT = auto()
    READ = auto()
    RETURN = auto()
    STRING = auto()
    EOF = auto()
    LETTER = auto()
    NEW_LINE_ESC = auto()
    TAB_ESC = auto()


Lexeme = namedtuple('Lexeme', ("token", "lexeme", "value", "line", "col"))


class Lexer:
    def __init__(self, file=sys.stdin):
        self.file = file
        self.line_num = 1
        self.col_num = 0
        self.cur_char = None
        self.current = None

        # start looking at the first character
        self.consume()


    def next(self):
        """
        Advance the lexer to the next token.

        Return the token
        """
        # self.end_line_and_tab()
        self.skip_space_and_comments()

        # check for eof
        if not self.cur_char:
            self.current = Lexeme(Token.EOF, '', None, 0, 0)
            return self.current

        if self.lex_single():
            return self.current
        elif self.lex_multi_fixed():
            return self.current
        elif self.lex_other():
            return self.current
        else:
            self.current = Lexeme(Token.INVALID, self.cur_char, None, self.line_num, self.col_num)
            self.consume()
            return self.current



    def consume(self):
        self.cur_char = self.file.read(1)
        self.col_num += 1
        if self.cur_char == '\n':
            self.line_num += 1
            self.col_num = 0
            
    #Considering tab contains 4 spaces.
        if self.cur_char == '\t':
            self.col_num += 4

    # def end_line_and_tab(self):
    #     if self.cur_char == "\\":
    #         self.consume()
    #         if self.cur_char == 'n':
    #             self.line_num += 1
    #             self.col_num = 0
    #         elif self.cur_char == 't':
    #             self.col_num += 4

                
    def skip_space_and_comments(self):
        """
        Consumes characters until we encounter non-whitespace.
        Also, skips comments.
        Also, stops on end of file.
        """
        while self.cur_char.isspace() or self.cur_char == '#':
            if self.cur_char == '#':
                # consume the rest of the line
                while self.cur_char != '\n':
                    self.consume()

            # consume all the whitespace
            while self.cur_char.isspace():
                self.consume()


    def lex_single(self):
        """
        Attempt to match our single character tokens.
        Return True on a match, False otherwise
        """
        lexemes = (("=", Token.EQUAL),
                   ("+", Token.PLUS),
                   ("-", Token.MINUS),
                   ("/", Token.DIVIDE),
                   (".", Token.DOT),
                   ("(", Token.LPAREN),
                   (")", Token.RPAREN),
                   ("[", Token.LSQUAREBRACKET),
                   ("]", Token.RSQUAREBRACKET),
                   (",", Token.COMMA),
                   ("_", Token.UNDERSCORE)
                   )

        for l in lexemes:
            if l[0] == self.cur_char:
                self.current = Lexeme(l[1], l[0], None, self.line_num, self.col_num)
                self.consume()
                return True
        return False


    def lex_multi_fixed(self):
        """
        Recognize multiple character tokens of fixed length.
        These tokens may share a common prefix.

        Basic Strategy:
            Find the longest string for which exactly one token
            exists.
              - Scan until we find an inconsistent character.
              - Make sure that we have an exact match.
        """
        lexemes = [ ("*", Token.TIMES),
                    ("**", Token.EXPONENT),
                    ("~=", Token.APPROXEQUAL),
                    (":=", Token.ASSIGN),
                    (":=:", Token.SWAP),
                    ('=', Token.EQUAL),
                    ("<", Token.LESSTHAN),
                    ("<=", Token.LTEQUAL),
                    (">", Token.GREATERTHAN),
                    (">=", Token.GTEQUAL),
                ]

        if self.cur_char == '"':
            self.lex_String_Build()
            return True 
                   
        if self.cur_char == "'":
            self.lex_Char_Build()
            return True

        # make a note of where we are in the file (token's beginning)
        line = self.line_num
        col = self.col_num


        # the accumulated lexeme string
        lstr = ''
        while len(lexemes) > 0:
            # try adding this to the token list
            trial = lstr + self.cur_char

            # find the lexemes that are consistent with trial
            consistent = [ l for l in lexemes if l[0].startswith(trial) ]

            # we stop if this character would make the token inconsisten
            if len(consistent) == 0:
                break
            else:
                lexemes = consistent
                lstr = trial
                self.consume()

        # make sure that at least one character matches
        if len(lstr) == 0:
            return False

        # try to make an exact match for lstr
        lexemes = [ l for l in lexemes if l[0] == lstr ]
        if len(lexemes) == 0:
            t = Token.INVALID
        else:
            t = lexemes[0][1]

        # build the token
        self.current = Lexeme(t, lstr, None, line, col)

        return True


    def lex_other(self):
        """
        Our only strategy is to build a state machine.
        """

         
        if self.cur_char == '.' or self.cur_char.isdigit():
            return self.lex_number()
        elif self.cur_char.isalpha() or self.cur_char == "_":
            return self.lex_kw_ref()
        return False

    def lex_String_Build(self):
        """
            Returns the lexeme as STRING
        """
        value = ""
        lexeme = self.cur_char
        col = self.col_num
        self.consume()

        while self.cur_char != '"':
            if not self.cur_char:
                self.current = Lexeme(Token.INVALID, lexeme, value, self.line_num, col)
                return True
            value = value + self.cur_char
            lexeme = lexeme + self.cur_char
            self.consume()
        
        # if self.cur_char == '"':
        #     self.consume()
        #     col = self.col_num
        #     while self.cur_char!='"':
        #         cur_char = self.cur_char
        #         if cur_char!='"':
        #             string_val = string_val + cur_char
        #             self.consume()
          
        #     self.consume()
        # string_lexeme = "\"" + string_val + "\""
        lexeme = lexeme + self.cur_char
        self.current = Lexeme(Token.STRING, lexeme, value, self.line_num, col)
        self.consume()
      

    def lex_Char_Build(self):
        """
            Returns the lexeme as CHARLIT
        """
        self.consume()
        char_value = ""

        col = self.col_num

        while self.cur_char and self.cur_char != "'":
            char_value = char_value + self.cur_char
            self.consume()
        
        char_lexeme = "\"" + char_value + "\""

        if self.cur_char == "'" and len(char_value) == 1:
            self.current = Lexeme(Token.CHARLIT, char_lexeme, char_value, self.line_num, self.col_num)
            self.consume()
        elif char_value == '\\n' and self.cur_char == "'":
            self.current = Lexeme(Token.NEW_LINE_ESC, char_value, '\n',self.line_num, col)
            self.consume()
        elif char_value == '\\t' and self.cur_char == "'":
            self.current = Lexeme(Token.TAB_ESC, char_value, '\t',self.line_num, col)
            self.consume()
        else:
            self.current = Lexeme(Token.INVALID, char_value, char_value, self.line_num, self.col_num)
            self.consume()



    def lex_number(self):
        # accumulate characters until they are inconsistent
        lstr = ''
        line = self.line_num
        col = self.col_num

        # assume an integer
        t = Token.INTLIT

        # accumulate all the digits
        while self.cur_char.isdigit():
            lstr += self.cur_char
            self.consume()

        if self.cur_char == '.':
            # this is a float
            t = Token.FLOATLIT

            lstr += self.cur_char
            self.consume()

            # grab the final digits
            while self.cur_char.isdigit():
                lstr += self.cur_char
                self.consume()

        #validate and extract the value
        if lstr[-1] == '.':
            t = Token.INVALID
            v = None
        elif t == Token.INTLIT:
            v = int(lstr)
        else:
            v = float(lstr)

        self.current = Lexeme(t, lstr, v, line, col)
        return True

    def lex_kw_ref(self):
        # a list of keywords
        kw = [ ('PROC', Token.PROC),
               ('WHILE', Token.WHILE), 
               ('IF', Token.IF),
               ('BEGIN', Token.BEGIN), 
               ('END', Token.END),
               ('PRINT', Token.PRINT), 
               ('RETURN', Token.RETURN),
               ('NUMBER', Token.NUMBER), 
               ('CHARACTER', Token.CHARACTER),
               ('ELSE', Token.ELSE), 
               ('READ', Token.READ),
               ('OBJECTOF', Token.OBJECTOF),
               ('BLUEPRINT', Token.BLUEPRINT),
               ('NOACCESS', Token.NOACCESS),
               ('CHILDOF', Token.CHILDOF),
               ('VOID', Token.VOID)
            ]

        # accumulate all the alpha numeric characters and _
        lstr = ''
        line = self.line_num
        col = self.col_num

        while self.cur_char.isalnum() or self.cur_char == "_":
            lstr += self.cur_char
            self.consume()
        
        # if (len(lstr) == 1 and lstr.isalpha()): 
        #     self.current = Lexeme(Token.LETTER, lstr, lstr, line, col)
        #     return True 

        # search for keyword or make it a ref
        kw = [ w for w in kw if w[0] == lstr ]
        if len(kw) == 1:
            t = kw[0][1]
        else:
            t = Token.REF

        self.current = Lexeme(t, lstr, None, line, col)
        return True

if __name__ == '__main__':
    lexer = Lexer(open(sys.argv[1]))
    while lexer.cur_char != '':
        print(lexer.next())
